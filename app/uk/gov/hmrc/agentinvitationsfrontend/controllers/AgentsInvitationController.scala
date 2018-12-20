/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.{Inject, Named, Singleton}
import org.joda.time.LocalDate
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation._
import play.api.data.{Form, Mapping}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{CheckDetailsPageConfig, DeletePageConfig, ReviewAuthorisationsPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

@Singleton
class AgentsInvitationController @Inject()(
  @Named("agent-invitations-frontend.external-url") externalUrl: String,
  @Named("agent-services-account-frontend.external-url") asAccUrl: String,
  invitationsService: InvitationsService,
  auditService: AuditService,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  continueUrlCache: ContinueUrlCache,
  val messagesApi: play.api.i18n.MessagesApi,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags)
    extends FrontendController with I18nSupport with AuthActions {

  import AgentsInvitationController._
  import continueUrlActions._

  private val personalIncomeRecord =
    if (featureFlags.showPersonalIncome)
      Seq(HMRCPIR -> Messages("personal-select-service.personal-income-viewer"))
    else Seq.empty
  private val mtdItId =
    if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("personal-select-service.itsa")) else Seq.empty
  private val vat =
    if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("personal-select-service.vat")) else Seq.empty

  private val personalOption = Seq("personal" -> Messages("client-type.personal"))
  private val businessOption = Seq("business" -> Messages("client-type.business"))
  private val clientTypes = personalOption ++ businessOption

  private def enabledServices(isWhitelisted: Boolean): Seq[(String, String)] =
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }

  private val serviceToMessageKey: String => String = {
    case HMRCMTDIT  => messageKeyForITSA
    case HMRCPIR    => messageKeyForAfi
    case HMRCMTDVAT => messageKeyForVAT
    case _          => "Service is missing"
  }

  private[controllers] val isDevEnv =
    if (env.mode.equals(Mode.Test)) false else configuration.getString("run.mode").forall(Mode.Dev.toString.equals)
  private[controllers] val agentServicesAccountUrl: String =
    if (isDevEnv) s"http://localhost:9401/agent-services-account" else "/agent-services-account"

  val agentInvitationIdentifyKnownFactForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackGenericFormKnownFact(featureFlags)

  val agentInvitationIdentifyClientFormItsa: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationIdentifyClientFormItsa(featureFlags)

  val agentInvitationPostCodeForm: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

  val agentInvitationIdentifyClientFormVat: Form[UserInputVrnAndRegDate] =
    AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)

  val agentInvitationIdentifyClientFormIrv: Form[UserInputNinoAndDob] =
    AgentsInvitationController.agentInvitationIdentifyClientFormIrv(featureFlags)

  val agentFastTrackPostcodeForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackKnownFactForm(featureFlags, postcodeMapping(featureFlags))

  val agentFastTrackDateOfBirthForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackKnownFactForm(featureFlags, dateOfBirthMapping(featureFlags))

  val agentFastTrackVatRegDateForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackKnownFactForm(featureFlags, vatRegDateMapping(featureFlags))

  val agentsRoot: Action[AnyContent] = ActionWithMdc { implicit request =>
    Redirect(routes.AgentsInvitationController.showClientType())
  }

  val showClientType: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.fetchAndClear
      currentAuthorisationRequestCache.fetch.map {
        case Some(data) if data.clientType.isEmpty && data.fromFastTrack =>
          Ok(client_type(agentInvitationSelectClientTypeForm, clientTypes, agentServicesAccountUrl))
        case _ =>
          currentAuthorisationRequestCache.fetchAndClear
          Ok(client_type(agentInvitationSelectClientTypeForm, clientTypes, agentServicesAccountUrl))
      }
    }
  }

  val submitClientType: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentInvitationSelectClientTypeForm
        .bindFromRequest()
        .fold(
          formWithErrors => Future successful Ok(client_type(formWithErrors, clientTypes, agentServicesAccountUrl)),
          userInput => {
            val updateAggregate = currentAuthorisationRequestCache.fetch
              .map(_.getOrElse(CurrentAuthorisationRequest()))
              .map(_.copy(clientType = userInput.clientType))

            updateAggregate.flatMap(
              updateFastTrack =>
                currentAuthorisationRequestCache
                  .save(updateFastTrack)
                  .flatMap(_ => redirectBasedOnCurrentInputState(arn, updateFastTrack, isWhitelisted)))

          }
        )
    }
  }

  val showSelectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      journeyStateCache.fetch.flatMap {
        case Some(basket) if basket.requests.nonEmpty =>
          basket.clientType match {
            case "personal" =>
              Future successful Ok(
                personal_select_service(agentInvitationServiceForm, enabledServices(isWhitelisted), true))
            case "business" => Future successful Ok(business_select_service(agentInvitationServiceForm))
            case _ => {
              Future successful Redirect(routes.AgentsInvitationController.showClientType())
            }
          }
        case _ =>
          currentAuthorisationRequestCache.fetch.flatMap {
            case Some(input) if input.clientType.nonEmpty =>
              input.clientType match {
                case `personal` =>
                  Future successful Ok(
                    personal_select_service(agentInvitationServiceForm, enabledServices(isWhitelisted), false))
                case `business` =>
                  Future successful Ok(business_select_service(agentInvitationServiceForm))
                case _ => {
                  Future successful Redirect(routes.AgentsInvitationController.showClientType())
                }
              }
            case _ => {
              Future successful Redirect(routes.AgentsInvitationController.showClientType())
            }
          }
      }
    }
  }

  def submitSelectServicePersonal(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] = {
    val allowedServices = enabledServices(isWhitelisted)
    agentInvitationServiceForm
      .bindFromRequest()
      .fold(
        formWithErrors => {
          for {
            authorisationBasket <- journeyStateCache.fetch
          } yield Ok(personal_select_service(formWithErrors, allowedServices, authorisationBasket.isDefined))
        },
        userInput => {
          val updateAggregate = currentAuthorisationRequestCache.fetch
            .map(_.getOrElse(CurrentAuthorisationRequest()))
            .map(_.copy(service = userInput.service))

          updateAggregate.flatMap(
            updateFastTrack =>
              currentAuthorisationRequestCache
                .save(updateFastTrack)
                .flatMap(_ =>
                  ifShouldShowService(updateFastTrack, featureFlags, isWhitelisted) {
                    redirectBasedOnCurrentInputState(arn, updateFastTrack, isWhitelisted)
                }))
        }
      )
  }

  def submitSelectServiceBusiness(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    agentInvitationBusinessServiceForm
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(business_select_service(formWithErrors))
        },
        userInput => {
          if (userInput.service == HMRCMTDVAT) {
            val updateAggregate = currentAuthorisationRequestCache.fetch
              .map(_.getOrElse(CurrentAuthorisationRequest()))
              .map(_.copy(service = HMRCMTDVAT))

            updateAggregate.flatMap(
              updateFastTrack =>
                currentAuthorisationRequestCache
                  .save(updateFastTrack)
                  .flatMap(_ =>
                    ifShouldShowService(updateFastTrack, featureFlags, isWhitelisted) {
                      redirectBasedOnCurrentInputState(arn, updateFastTrack, isWhitelisted)
                  }))

          } else {
            for {
              _      <- currentAuthorisationRequestCache.save(CurrentAuthorisationRequest())
              result <- Future successful Redirect(routes.AgentsInvitationController.showClientType())
            } yield result
          }
        }
      )

  val submitSelectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      clientTypeOnlyForm
        .bindFromRequest()
        .fold(
          _ => Future successful Redirect(routes.AgentsInvitationController.showClientType()), {
            case Some("personal") => submitSelectServicePersonal(arn, isWhitelisted)
            case Some("business") => submitSelectServiceBusiness(arn, isWhitelisted)
            case _                => Future successful Redirect(routes.AgentsInvitationController.showClientType())
          }
        )
    }
  }

  private val authorisationRequestToIdentifyKnownFact = (authorisationRequest: CurrentAuthorisationRequest) => {
    val service = authorisationRequest.service
    val clientId = authorisationRequest.clientIdentifier
    val clientIdType = authorisationRequest.clientIdentifierType
    agentInvitationIdentifyKnownFactForm.fill(
      CurrentAuthorisationRequest(
        authorisationRequest.clientType,
        service,
        clientIdType,
        clientId,
        authorisationRequest.knownFact)
    )

  }

  private val authorisationRequestToIdentifyClientFormItsa = (authorisationRequest: CurrentAuthorisationRequest) => {
    val service = authorisationRequest.service
    val clientId = authorisationRequest.clientIdentifier
    agentInvitationIdentifyClientFormItsa.fill(
      UserInputNinoAndPostcode(
        authorisationRequest.clientType,
        service,
        Some(clientId),
        authorisationRequest.knownFact))
  }

  private val authorisationRequestToIdentifyClientFormVat = (authorisationRequest: CurrentAuthorisationRequest) => {
    val service = authorisationRequest.service
    val clientId = authorisationRequest.clientIdentifier
    agentInvitationIdentifyClientFormVat.fill(
      UserInputVrnAndRegDate(authorisationRequest.clientType, service, Some(clientId), authorisationRequest.knownFact))
  }

  private val authorisationRequestToIdentifyClientFormIrv = (authorisationRequest: CurrentAuthorisationRequest) => {
    val service = authorisationRequest.service
    val clientId = authorisationRequest.clientIdentifier
    agentInvitationIdentifyClientFormIrv.fill(
      UserInputNinoAndDob(authorisationRequest.clientType, service, Some(clientId), authorisationRequest.knownFact))
  }

  val showIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.map {
        case Some(inviteDetails) if inviteDetails.service.nonEmpty =>
          inviteDetails.service match {
            case HMRCMTDIT =>
              Ok(
                identify_client_itsa(
                  authorisationRequestToIdentifyClientFormItsa(inviteDetails),
                  featureFlags.showKfcMtdIt,
                  inviteDetails.fromFastTrack))
            case HMRCMTDVAT =>
              Ok(
                identify_client_vat(
                  authorisationRequestToIdentifyClientFormVat(inviteDetails),
                  featureFlags.showKfcMtdVat,
                  inviteDetails.fromFastTrack))
            case HMRCPIR =>
              Ok(
                identify_client_irv(
                  authorisationRequestToIdentifyClientFormIrv(inviteDetails),
                  featureFlags.showKfcPersonalIncome,
                  inviteDetails.fromFastTrack))
            case _ => Redirect(routes.AgentsInvitationController.showClientType())
          }
        case _ => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      serviceNameForm
        .bindFromRequest()
        .fold(
          _ => Future successful Redirect(routes.AgentsInvitationController.showSelectService()), {
            case HMRCMTDIT  => identifyItsaClient(arn, isWhitelisted)
            case HMRCMTDVAT => identifyVatClient(arn, isWhitelisted)
            case HMRCPIR    => identifyIrvClient(arn, isWhitelisted)
            case _          => Future successful Redirect(routes.AgentsInvitationController.showClientType())
          }
        )
    }
  }

  val showCheckDetails: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.map {
        case Some(currentInvitation) =>
          Ok(
            check_details(
              checkDetailsForm,
              currentInvitation,
              featureFlags,
              serviceToMessageKey(currentInvitation.service),
              CheckDetailsPageConfig(currentInvitation, featureFlags)))
        case None => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitCheckDetails: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      val cachedCurrentInvitationInput =
        currentAuthorisationRequestCache.fetch.map(_.getOrElse(CurrentAuthorisationRequest()))
      checkDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors => {
            cachedCurrentInvitationInput.flatMap { cii =>
              Future successful Ok(
                check_details(
                  formWithErrors,
                  cii,
                  featureFlags,
                  serviceToMessageKey(cii.service),
                  CheckDetailsPageConfig(cii, featureFlags)))
            }
          },
          data => {
            if (data.value.getOrElse(false)) {
              cachedCurrentInvitationInput.flatMap { cii =>
                redirectBasedOnCurrentInputState(arn, cii, isWhitelisted)
              }
            } else
              Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          }
        )
    }
  }

  val showConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(data) =>
          (data.clientIdentifier, data.service) match {
            case (clientId, service) if clientId.nonEmpty =>
              invitationsService.getClientNameByService(clientId, service).flatMap { name =>
                Future successful Ok(
                  confirm_client(name.getOrElse(""), agentConfirmationForm("error.confirm-client.required")))
              }
            case _ => Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          }
        case None => Future successful Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(invitationWithClientDetails) =>
          (
            invitationWithClientDetails.clientType,
            invitationWithClientDetails.clientIdentifier,
            invitationWithClientDetails.service) match {
            case (clientType, clientId, service) if clientId.nonEmpty =>
              invitationsService.getClientNameByService(clientId, service).flatMap { name =>
                val clientName = name.getOrElse("")
                agentConfirmationForm("error.confirm-client.required")
                  .bindFromRequest()
                  .fold(
                    formWithErrors => Future successful Ok(confirm_client(clientName, formWithErrors)),
                    data =>
                      if (data.choice) {
                        for {
                          journeyStateOpt <- journeyStateCache.fetch
                          currentCache = journeyStateOpt match {
                            case None               => AgentMultiAuthorisationJourneyState("", Set.empty)
                            case Some(journeyState) => journeyState
                          }

                          hasPendingInvitations <- invitationsService.hasPendingInvitationsFor(arn, clientId, service)
                          result <- if (hasPendingInvitations) {
                                     Future successful Redirect(
                                       routes.AgentsInvitationController.pendingAuthorisationExists())
                                   } else
                                     for {
                                       _ <- journeyStateCache.save(AgentMultiAuthorisationJourneyState(
                                             if (currentCache.clientType.nonEmpty) currentCache.clientType
                                             else invitationWithClientDetails.clientType.getOrElse(""),
                                             currentCache.requests ++ Seq(
                                               AuthorisationRequest(clientName, service, clientId))
                                           ))
                                       redirect <- if (invitationWithClientDetails.clientType == personal || currentCache.clientType == "personal")
                                                    Future successful Redirect(
                                                      routes.AgentsInvitationController.showReviewAuthorisations())
                                                  else if (invitationWithClientDetails.clientType == business) {
                                                    confirmAndRedirect(arn, invitationWithClientDetails, false)
                                                  } else
                                                    Future successful Redirect(
                                                      routes.AgentsInvitationController.showClientType())
                                     } yield redirect
                        } yield result
                      } else {
                        for {
                          _      <- currentAuthorisationRequestCache.save(CurrentAuthorisationRequest(clientType, service))
                          result <- Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
                        } yield result
                    }
                  )
              }
            case _ => Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          }
        case None => Future successful Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  def showReviewAuthorisations: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      for {
        journeyStateOpt <- journeyStateCache.fetch
        result = journeyStateOpt match {
          case Some(journeyState) if journeyState.requests.nonEmpty =>
            Ok(
              review_authorisations(
                ReviewAuthorisationsPageConfig(journeyState),
                agentConfirmationForm("error.review-authorisation.required")))
          case Some(_) => Redirect(routes.AgentsInvitationController.allAuthorisationsRemoved())
          case None    => Redirect(routes.AgentsInvitationController.showClientType())
        }
      } yield result
    }
  }

  def submitReviewAuthorisations: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      journeyStateCache.get.flatMap { journeyState =>
        agentConfirmationForm("error.review-authorisation.required")
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Future.successful(
                Ok(review_authorisations(ReviewAuthorisationsPageConfig(journeyState), formWithErrors))),
            input => {
              if (input.choice) {
                currentAuthorisationRequestCache.save(CurrentAuthorisationRequest(Some(journeyState.clientType))) map (
                  _ => Redirect(routes.AgentsInvitationController.showSelectService()))
              } else {
                for {
                  processedRequests <- invitationsService
                                        .createMultipleInvitations(
                                          arn,
                                          journeyState.clientType,
                                          journeyState.requests,
                                          featureFlags)
                  _ <- journeyStateCache.save(journeyState.copy(requests = processedRequests))
                  result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests))
                             Future successful Redirect(routes.AgentsInvitationController.showInvitationSent())
                           else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                             Future successful Redirect(routes.AgentsErrorController.allCreateAuthorisationFailed())
                           else Future successful Redirect(routes.AgentsErrorController.someCreateAuthorisationFailed())
                } yield result
              }
            }
          )
      }
    }
  }

  def showDelete(itemId: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.get.map { journeyState =>
        val deleteItem =
          journeyState.requests.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
        Ok(delete(DeletePageConfig(deleteItem), agentConfirmationForm("error.delete.radio")))
      }
    }
  }

  def submitDelete(itemId: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      agentConfirmationForm("error.delete.radio")
        .bindFromRequest()
        .fold(
          formWithErrors => {
            journeyStateCache.fetch.map {
              case Some(journeyState) =>
                val deleteItem =
                  journeyState.requests.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
                Ok(delete(DeletePageConfig(deleteItem), formWithErrors))
              case None => Redirect(routes.AgentsInvitationController.showClientType())
            }
          },
          input =>
            if (input.choice) {
              journeyStateCache
                .transform(item => item.copy(requests = item.requests.filterNot(_.itemId == itemId)))
                .map { _ =>
                  Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
                }
            } else {
              Future successful Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
          }
        )
    }
  }

  private[controllers] def redirectOrShowConfirmClient(
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    featureFlags: FeatureFlags)(body: => Future[Result])(implicit request: Request[_]): Future[Result] =
    if (currentAuthorisationRequest.fromFastTrack) body
    else {
      currentAuthorisationRequest.service match {
        case HMRCMTDIT if featureFlags.enableMtdItToConfirm =>
          Future successful Redirect(routes.AgentsInvitationController.showConfirmClient())
        case HMRCMTDVAT if featureFlags.enableMtdVatToConfirm =>
          Future successful Redirect(routes.AgentsInvitationController.showConfirmClient())
        case HMRCPIR if featureFlags.enableIrvToConfirm =>
          Future successful Redirect(routes.AgentsInvitationController.showConfirmClient())
        case _ => {
          for {
            clientName <- invitationsService.getClientNameByService(
                           currentAuthorisationRequest.clientIdentifier,
                           currentAuthorisationRequest.service)
            journeyStateOpt <- journeyStateCache.fetch
            currentCache = journeyStateOpt match {
              case None               => AgentMultiAuthorisationJourneyState("", Set.empty)
              case Some(journeyState) => journeyState
            }
            _ <- journeyStateCache.save(
                  AgentMultiAuthorisationJourneyState(
                    currentAuthorisationRequest.clientType.getOrElse(""),
                    currentCache.requests ++ Set(
                      AuthorisationRequest(
                        clientName.getOrElse(""),
                        currentAuthorisationRequest.service,
                        currentAuthorisationRequest.clientIdentifier))
                  ))
            result <- currentAuthorisationRequest.clientType match {
                       case `personal` =>
                         Future successful Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
                       case `business` => body
                       case _          => Future.successful(Redirect(routes.AgentsInvitationController.showClientType()))
                     }
          } yield result
        }
      }
    }

  val showKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.map {
        case Some(currentInvitation)
            if currentInvitation.clientIdentifier.nonEmpty && currentInvitation.clientIdentifierType.nonEmpty =>
          Ok(
            known_fact(
              authorisationRequestToIdentifyKnownFact(currentInvitation),
              serviceToMessageKey(currentInvitation.service)))
        case Some(_) => throw new Exception("no content in cache")
        case None    => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      serviceNameForm
        .bindFromRequest()
        .fold(
          _ => {
            Future successful Redirect(routes.AgentsInvitationController.showClientType())
          }, {
            case "HMRC-MTD-IT" =>
              bindKnownFactForm(agentFastTrackPostcodeForm, arn, isWhitelisted, "itsa")
            case "PERSONAL-INCOME-RECORD" =>
              bindKnownFactForm(agentFastTrackDateOfBirthForm, arn, isWhitelisted, "afi")
            case "HMRC-MTD-VAT" =>
              bindKnownFactForm(agentFastTrackVatRegDateForm, arn, isWhitelisted, "vat")
          }
        )
    }
  }

  def bindKnownFactForm(
    knownFactForm: Form[CurrentAuthorisationRequest],
    arn: Arn,
    isWhitelisted: Boolean,
    serviceMessageKey: String)(implicit request: Request[AnyContent]): Future[Result] =
    knownFactForm
      .bindFromRequest()
      .fold(
        formWithErrors => Future successful Ok(known_fact(formWithErrors, serviceMessageKey)),
        data => redirectBasedOnCurrentInputState(arn, data.copy(fromFastTrack = true), isWhitelisted)
      )

  def identifyItsaClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[Result] =
    agentInvitationIdentifyClientFormItsa
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(identify_client_itsa(formWithErrors, featureFlags.showKfcMtdIt, true))
        },
        userInput =>
          for {
            maybeCachedInvitation <- currentAuthorisationRequestCache.fetch
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentAuthorisationRequest())
              .copy(
                clientIdentifier = userInput.clientIdentifier.getOrElse(""),
                clientIdentifierType = "ni",
                knownFact = userInput.postcode
              )
            _              <- currentAuthorisationRequestCache.save(invitationWithClientDetails)
            redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
          } yield redirectResult
      )

  def identifyVatClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[Result] =
    agentInvitationIdentifyClientFormVat
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(identify_client_vat(formWithErrors, featureFlags.showKfcMtdVat, true))
        },
        userInput =>
          for {
            maybeCachedInvitation <- currentAuthorisationRequestCache.fetch
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentAuthorisationRequest())
              .copy(
                clientIdentifier = userInput.clientIdentifier.getOrElse(""),
                clientIdentifierType = "vrn",
                knownFact = userInput.registrationDate
              )
            _              <- currentAuthorisationRequestCache.save(invitationWithClientDetails)
            redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
          } yield redirectResult
      )

  def identifyIrvClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[Result] =
    agentInvitationIdentifyClientFormIrv
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(
            identify_client_irv(
              formWithErrors,
              featureFlags.showKfcPersonalIncome,
              true
            ))
        },
        userInput =>
          for {
            maybeCachedInvitation <- currentAuthorisationRequestCache.fetch
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentAuthorisationRequest())
              .copy(
                clientIdentifier = userInput.clientIdentifier.getOrElse(""),
                clientIdentifierType = "ni",
                knownFact = userInput.dob
              )
            hasPendingInvitations <- if (invitationWithClientDetails.service.nonEmpty && invitationWithClientDetails.clientIdentifier.nonEmpty)
                                      invitationsService.hasPendingInvitationsFor(
                                        arn,
                                        invitationWithClientDetails.clientIdentifier,
                                        invitationWithClientDetails.service)
                                    else Future.successful(false)
            result <- if (hasPendingInvitations) {
                       Future successful Redirect(routes.AgentsInvitationController.pendingAuthorisationExists())
                     } else
                       for {
                         _ <- currentAuthorisationRequestCache.save(invitationWithClientDetails)
                         redirectResult <- redirectBasedOnCurrentInputState(
                                            arn,
                                            invitationWithClientDetails,
                                            isWhitelisted)
                       } yield redirectResult
          } yield result
      )

  private[controllers] def createInvitation[T <: TaxIdentifier](arn: Arn, fti: FastTrackInvitation[T])(
    implicit request: Request[_]) =
    for {
      _         <- invitationsService.createInvitation(arn, fti, featureFlags)
      multiLink <- invitationsService.createAgentLink(arn, fti.clientType.getOrElse(""))
    } yield Redirect(routes.AgentsInvitationController.showInvitationSent())

  val showInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      journeyStateCache.get.flatMap(cacheItem =>
        for {
          agentLink <- invitationsService.createAgentLink(arn, cacheItem.clientType)
          _         <- currentAuthorisationRequestCache.save(CurrentAuthorisationRequest())
          continue  <- continueUrlCache.fetch
        } yield {
          val invitationUrl: String = s"$externalUrl$agentLink"
          val clientType = if (agentLink.contains("personal")) "personal" else "business"
          Ok(invitation_sent(invitationUrl, continue.isDefined, featureFlags.enableTrackRequests, clientType))
      })
    }
  }

  val continueAfterInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      for {
        continue <- continueUrlCache.fetch.map(continue => continue.getOrElse(ContinueUrl(agentServicesAccountUrl)))
        _        <- continueUrlCache.remove()
      } yield Redirect(continue.url)
    }
  }

  val notEnrolled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetchAndClear.map {
        case Some(aggregate) =>
          aggregate.service match {
            case HMRCMTDVAT =>
              Forbidden(not_enrolled(Services.messageKeyForVAT))
            case HMRCMTDIT =>
              Forbidden(not_enrolled(Services.messageKeyForITSA))
            case _ =>
              throw new Exception("Unsupported Service")
          }
        case None => throw new Exception("Empty Cache")
      }
    }
  }

  val allAuthorisationsRemoved: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(all_authorisations_removed())
    }
  }

  val pendingAuthorisationExists: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(pending_authorisation_exists())
    }
  }

  private[controllers] def confirmAndRedirect(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    currentAuthorisationRequest match {
      case CurrentInvitationInputItsaReady(completeItsaInvitation) =>
        createInvitation(arn, completeItsaInvitation)
      case CurrentInvitationInputVatReady(completeVatInvitation) =>
        createInvitation(arn, completeVatInvitation)
      case CurrentInvitationInputPirReady(completeIrvInvitation) =>
        createInvitation(arn, completeIrvInvitation)
      case _ =>
        Logger(getClass).warn(s"Something has gone wrong. Redirected back to check current state.")
        redirectBasedOnCurrentInputState(arn, currentAuthorisationRequest, isWhitelisted)
    }

  val agentFastTrack: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      if (featureFlags.enableFastTrack) {
        agentFastTrackForm
          .bindFromRequest()
          .fold(
            formErrors => {
              withMaybeErrorUrlCached {
                case Some(continue) =>
                  Future successful Redirect(
                    continue.url + s"?issue=${formErrors.errorsAsJson.as[FastTrackErrors].formErrorsMessages}")
                case None =>
                  throw new IllegalStateException("No Error Url Provided")
              }
            },
            currentAuthorisationRequest => {
              val authorisationRequest = currentAuthorisationRequest.copy(fromFastTrack = true)
              currentAuthorisationRequestCache.save(authorisationRequest).flatMap { _ =>
                withMaybeContinueUrlCached {
                  ifShouldShowService(authorisationRequest, featureFlags, isWhitelisted) {
                    Future successful Redirect(routes.AgentsInvitationController.showCheckDetails())
                  }
                }
              }
            }
          )
      } else {
        Logger(getClass).warn("Fast-Track feature flag is switched off")
        Future successful BadRequest
      }
    }
  }

  private[controllers] def knownFactCheckVat(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    fastTrackVatInvitation: FastTrackVatInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    fastTrackVatInvitation.vatRegDate.map(date => LocalDate.parse(date.value)) match {
      case Some(vatRegDate) =>
        invitationsService
          .checkVatRegistrationDateMatches(fastTrackVatInvitation.clientIdentifier, vatRegDate) flatMap {
          case Some(true) =>
            redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
              createInvitation(arn, fastTrackVatInvitation)
            }
          case Some(false) =>
            currentAuthorisationRequestCache.save(currentAuthorisationRequest).map { _ =>
              Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Date Does Not Match.")
              Redirect(routes.AgentsErrorController.notMatched())
            }
          case None =>
            currentAuthorisationRequestCache.save(currentAuthorisationRequest).map { _ =>
              Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: VAT Registration Not Found.")
              Redirect(routes.AgentsInvitationController.notEnrolled())
            }
        }
      case None =>
        redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
          createInvitation(arn, fastTrackVatInvitation)
        }
    }

  private[controllers] def knownFactCheckItsa(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    fastTrackItsaInvitation: FastTrackItsaInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    fastTrackItsaInvitation.postcode match {
      case Some(postcode) =>
        for {
          hasPostcode <- invitationsService
                          .checkPostcodeMatches(fastTrackItsaInvitation.clientIdentifier, postcode.value)
          result <- hasPostcode match {
                     case Some(true) =>
                       redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
                         createInvitation(arn, fastTrackItsaInvitation)
                       }
                     case Some(false) =>
                       currentAuthorisationRequestCache.save(currentAuthorisationRequest).map { _ =>
                         Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
                         auditService.sendAgentInvitationSubmitted(
                           arn,
                           "",
                           fastTrackItsaInvitation,
                           "Fail",
                           Some("POSTCODE_DOES_NOT_MATCH"))
                         Redirect(routes.AgentsErrorController.notMatched())
                       }
                     case None =>
                       currentAuthorisationRequestCache.save(currentAuthorisationRequest).map { _ =>
                         Logger(getClass).warn(
                           s"${arn.value}'s Invitation Creation Failed: Client Registration Not Found.")
                         auditService.sendAgentInvitationSubmitted(
                           arn,
                           "",
                           fastTrackItsaInvitation,
                           "Fail",
                           Some("CLIENT_REGISTRATION_NOT_FOUND"))
                         Redirect(routes.AgentsInvitationController.notEnrolled())
                       }
                   }
        } yield result
      case None =>
        redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
          createInvitation(arn, fastTrackItsaInvitation)
        }
    }

  private[controllers] def knownFactCheckIrv(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    fastTrackPirInvitation: FastTrackPirInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]) =
    if (featureFlags.showKfcPersonalIncome) {
      fastTrackPirInvitation.dob match {
        case Some(dob) =>
          invitationsService
            .checkCitizenRecordMatches(fastTrackPirInvitation.clientIdentifier, LocalDate.parse(dob.value))
            .flatMap {
              case Some(true) =>
                redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
                  createInvitation(arn, fastTrackPirInvitation)
                }
              case Some(false) =>
                Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Not Matched from Citizen-Details.")
                Future successful Redirect(routes.AgentsErrorController.notMatched())
              case None =>
                Logger(getClass).warn(
                  s"${arn.value}'s Invitation Creation Failed: No Record found from Citizen-Details.")
                Future successful Redirect(routes.AgentsErrorController.notMatched())
            }
        case None =>
          Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: No KnownFact Provided")
          Future successful Redirect(routes.AgentsErrorController.notMatched())
      }
    } else {
      redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
        createInvitation(arn, fastTrackPirInvitation)
      }
    }

  def redirectBasedOnCurrentInputState(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    currentAuthorisationRequest match {
      case CurrentInvitationInputVatReady(completeVatInvitation) =>
        knownFactCheckVat(arn, currentAuthorisationRequest, completeVatInvitation, isWhitelisted)

      case CurrentInvitationInputItsaReady(completeItsaInvitation) =>
        knownFactCheckItsa(arn, currentAuthorisationRequest, completeItsaInvitation, isWhitelisted)

      case CurrentInvitationInputPirReady(completePirInvitation) =>
        knownFactCheckIrv(arn, currentAuthorisationRequest, completePirInvitation, isWhitelisted)

      case CurrentInvitationInputNeedsClientType(_) =>
        Future successful Redirect(routes.AgentsInvitationController.showClientType())

      case CurrentInvitationInputNeedsKnownFact(_) =>
        Future successful Redirect(routes.AgentsInvitationController.showKnownFact())

      case CurrentInvitationInputNeedsClientIdentifier(invitationNeedsClientIdentifier) =>
        invitationNeedsClientIdentifier.service match {
          case service if isSupportedWhitelistedService(service, isWhitelisted) =>
            Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          case _ =>
            Future successful Redirect(routes.AgentsInvitationController.showClientType())
        }

      case CurrentInvitationInputFromFastTrackNeedsClientType(_) =>
        Future successful Redirect(routes.AgentsInvitationController.showClientType())

      case CurrentInvitationInputNeedService(_) =>
        Future successful Redirect(routes.AgentsInvitationController.showSelectService())

      case _ =>
        Logger(getClass).warn("Resetting due to mix data in session")
        currentAuthorisationRequestCache
          .save(CurrentAuthorisationRequest())
          .map(_ => Redirect(routes.AgentsInvitationController.showClientType()))
    }

  private def ifShouldShowService(
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    featureFlags: FeatureFlags,
    isWhitelisted: Boolean)(body: => Future[Result]): Future[Result] =
    currentAuthorisationRequest.service match {
      case HMRCPIR if !isWhitelisted =>
        Logger(getClass).warn(s"User is not whitelisted to create $HMRCPIR invitation")
        Future successful BadRequest
      case HMRCMTDVAT if !featureFlags.showHmrcMtdVat =>
        Logger(getClass).warn(s"Service: $HMRCMTDVAT feature flagged is switched off")
        Future successful BadRequest
      case HMRCMTDIT if !featureFlags.showHmrcMtdIt =>
        Logger(getClass).warn(s"Service: $HMRCMTDIT feature flagged is switched off")
        Future successful BadRequest
      case HMRCPIR if !featureFlags.showPersonalIncome =>
        Logger(getClass).warn(s"Service: $HMRCPIR feature flagged is switched off")
        Future successful BadRequest
      case _ => body
    }

  private def isSupportedWhitelistedService(service: String, isWhitelisted: Boolean): Boolean =
    enabledServices(isWhitelisted).exists(_._1 == service)

  private def withMaybeContinueUrlCached[A](
    block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    withMaybeContinueUrl {
      case None      => block
      case Some(url) => continueUrlCache.save(url).flatMap(_ => block)
    }

  private def withMaybeErrorUrlCached[A](
    block: Option[ContinueUrl] => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    withMaybeErrorUrl {
      case None      => block(None)
      case Some(url) => continueUrlCache.cacheAndFetchErrorUrl(url).flatMap(urlOps => block(urlOps))
    }
}

object AgentsInvitationController {

  import DateFieldHelper._
  import Services._
  import ValidateHelper._

  private val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  private val postcodeCharactersRegex = "^[a-zA-Z0-9 ]+$"

  val detailsChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError("error.confirmDetails.invalid"))
  }

  private val servicePersonalChoice: Constraint[Option[String]] =
    radioChoice("error.service.required")

  private val serviceBusinessChoice: Constraint[Option[String]] =
    radioChoice("error.business-service.required")

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

  private val clientTypeChoice: Constraint[Option[String]] =
    radioChoice("error.client-type.required")

  private def confirmationChoice(errorMessage: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError(errorMessage))
  }

  val checkDetailsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("checkDetails" -> optional(boolean)
      .verifying(detailsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  private def validNino(
    nonEmptyFailure: String = "error.nino.required",
    invalidFailure: String = "enter-nino.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(nino => Nino.isValid(nino))

  private val validVrn =
    ValidateHelper.validateVrnField("error.vrn.required", "enter-vrn.regex-failure", "enter-vrn.checksum-failure")

  def validPostcode(
    isKfcFlagOn: Boolean,
    invalidFormatFailure: String,
    emptyFailure: String,
    invalidCharactersFailure: String) = Constraint[String] { input: String =>
    if (isKfcFlagOn) {
      if (input.isEmpty) Invalid(ValidationError(emptyFailure))
      else if (!input.matches(postcodeCharactersRegex)) Invalid(ValidationError(invalidCharactersFailure))
      else if (!input.matches(postcodeRegex)) Invalid(ValidationError(invalidFormatFailure))
      else Valid
    } else Valid
  }

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)
  val trimmedUppercaseText: Mapping[String] = of[String].transform(_.trim.toUpperCase, identity)
  val lowerCaseText: Mapping[String] = of[String].transform(_.trim.toLowerCase, identity)

  val clientTypeOnlyForm: Form[Option[String]] = Form(mapping("clientType" -> optional(text)
    .verifying("Unsupported Client Type", clientType => supportedClientTypes.contains(clientType)))(identity)(Some(_)))

  val serviceNameForm: Form[String] = Form(
    mapping("service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)))(
      identity)(Some(_)))

  def agentInvitationIdentifyClientFormItsa(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "clientType" -> optional(text),
        "service"    -> text,
        "clientIdentifier" -> normalizedText.verifying(
          validNino(nonEmptyFailure = "error.nino.required", invalidFailure = "enter-nino.invalid-format")),
        "knownFact" -> optionalIf(
          featureFlags.showKfcMtdIt,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showKfcMtdIt,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (clientType, service, clientIdentifier, postcode) =>
        UserInputNinoAndPostcode(clientType, service, Some(clientIdentifier.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

  def agentInvitationIdentifyClientFormVat(featureFlags: FeatureFlags): Form[UserInputVrnAndRegDate] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validVrn),
        "knownFact"        -> optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))
      )({ (clientType, service, clientIdentifier, registrationDate) =>
        UserInputVrnAndRegDate(clientType, service, Some(clientIdentifier.trim.toUpperCase()), registrationDate)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.registrationDate))
      }))

  def agentInvitationIdentifyClientFormIrv(featureFlags: FeatureFlags): Form[UserInputNinoAndDob] =
    Form(
      mapping(
        "clientType" -> optional(text),
        "service"    -> text,
        "clientIdentifier" -> normalizedText.verifying(
          validNino(nonEmptyFailure = "error.nino.required", invalidFailure = "enter-nino.invalid-format")),
        "knownFact" -> optionalIf(
          featureFlags.showKfcPersonalIncome,
          dateFieldsMapping(validDobDateFormat)
        )
      )({ (clientType, service, clientIdentifier, dob) =>
        UserInputNinoAndDob(clientType, service, Some(clientIdentifier.trim.toUpperCase()), dob)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.dob))
      }))

  val agentInvitationSelectClientTypeForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text).verifying(clientTypeChoice),
        "service"          -> text,
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, _, _, _) =>
        UserInputNinoAndPostcode(clientType, "", None, None)
      })({ user =>
        Some((user.clientType, "", None, None))
      })
    )
  }

  val agentInvitationServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> optional(text).verifying(servicePersonalChoice),
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, service, _, _) =>
        UserInputNinoAndPostcode(clientType, service.getOrElse(""), None, None)
      })({ user =>
        Some((user.clientType, Some(user.service), None, None))
      }))
  }

  val agentInvitationBusinessServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> optional(text).verifying(serviceBusinessChoice),
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, service, _, _) =>
        UserInputNinoAndPostcode(clientType, service.getOrElse(""), None, None)
      })({ user =>
        Some((user.clientType, Some(user.service), None, None))
      }))
  }

  def agentConfirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  def agentInvitationPostCodeForm(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText,
        "knownFact" -> optionalIf(
          featureFlags.showKfcMtdIt,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showKfcMtdIt,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (clientType, service, nino, postcode) =>
        UserInputNinoAndPostcode(clientType, service, Some(nino.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

  private val vrnRegex = "[0-9]{9}"
  private val ninoRegex = "[[A-Z]&&[^DFIQUV]][[A-Z]&&[^DFIQUVO]] ?\\d{2} ?\\d{2} ?\\d{2} ?[A-D]{1}"

  private[controllers] val validateClientId: Constraint[String] = Constraint[String] { fieldValue: String =>
    fieldValue match {
      case clientId if clientId.nonEmpty && clientId.matches(vrnRegex) =>
        if (Vrn.isValid(clientId)) Valid
        else Invalid(ValidationError("INVALID_VRN"))
      case clientId if clientId.nonEmpty && clientId.matches(ninoRegex) =>
        if (Nino.isValid(clientId)) Valid
        else Invalid(ValidationError("INVALID_NINO"))
      case _ =>
        Invalid(ValidationError(s"INVALID_CLIENT_ID_RECEIVED:${if (fieldValue.nonEmpty) fieldValue else "NOTHING"}"))
    }
  }

  private val validateFastTrackForm: Constraint[CurrentAuthorisationRequest] =
    Constraint[CurrentAuthorisationRequest] { formData: CurrentAuthorisationRequest =>
      formData match {
        case CurrentAuthorisationRequest(Some("personal") | None, HMRCMTDIT, "ni", clientId, _, _)
            if Nino.isValid(clientId) =>
          Valid
        case CurrentAuthorisationRequest(Some("personal") | None, HMRCPIR, "ni", clientId, _, _)
            if Nino.isValid(clientId) =>
          Valid
        case CurrentAuthorisationRequest(_, HMRCMTDVAT, "vrn", clientId, _, _) if Vrn.isValid(clientId) => Valid
        case _                                                                                          => Invalid(ValidationError("INVALID_SUBMISSION"))
      }
    }

  def agentFastTrackKnownFactForm(
    featureFlags: FeatureFlags,
    knownFactMapping: Mapping[Option[String]]): Form[CurrentAuthorisationRequest] =
    Form(
      mapping(
        "clientType"           -> optional(text),
        "service"              -> text,
        "clientIdentifierType" -> text,
        "clientIdentifier"     -> normalizedText,
        "knownFact"            -> knownFactMapping
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientType, service, clientIdType, clientId, knownFact)
      })({ authorisationRequest =>
        Some(
          (
            authorisationRequest.clientType,
            authorisationRequest.service,
            authorisationRequest.clientIdentifierType,
            authorisationRequest.clientIdentifier,
            authorisationRequest.knownFact))
      }).verifying(validateFastTrackForm))

  def postcodeMapping(featureFlags: FeatureFlags) =
    optionalIf(
      featureFlags.showKfcMtdIt,
      trimmedUppercaseText.verifying(
        validPostcode(
          featureFlags.showKfcMtdIt,
          "enter-postcode.invalid-format",
          "error.postcode.required",
          "enter-postcode.invalid-characters"))
    )

  def dateOfBirthMapping(featureFlags: FeatureFlags) =
    optionalIf(featureFlags.showKfcPersonalIncome, dateFieldsMapping(validDobDateFormat))

  def vatRegDateMapping(featureFlags: FeatureFlags) =
    optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))

  val agentFastTrackForm: Form[CurrentAuthorisationRequest] =
    Form(
      mapping(
        "clientType" -> optional(
          lowerCaseText.verifying("UNSUPPORTED_CLIENT_TYPE", Set("personal", "business").contains _)),
        "service" -> text.verifying("UNSUPPORTED_SERVICE", service => supportedServices.contains(service)),
        "clientIdentifierType" -> text
          .verifying("UNSUPPORTED_CLIENT_ID_TYPE", clientType => supportedTypes.contains(clientType)),
        "clientIdentifier" -> normalizedText.verifying(validateClientId),
        "knownFact"        -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientTypeFor(clientType, service), service, clientIdType, clientId, knownFact)
      })({ authorisationRequest =>
        Some(
          (
            authorisationRequest.clientType,
            authorisationRequest.service,
            authorisationRequest.clientIdentifierType,
            authorisationRequest.clientIdentifier,
            authorisationRequest.knownFact))
      }).verifying(validateFastTrackForm))

  def clientTypeFor(clientType: Option[String], service: String): Option[String] =
    clientType.orElse(service match {
      case "HMRC-MTD-IT"            => Some("personal")
      case "PERSONAL-INCOME-RECORD" => Some("personal")
      case _                        => None
    })

  def agentFastTrackGenericFormKnownFact(featureFlags: FeatureFlags): Form[CurrentAuthorisationRequest] =
    Form(
      mapping(
        "clientType" -> optional(text),
        "service"    -> text.verifying("UNSUPPORTED_SERVICE", service => supportedServices.contains(service)),
        "clientIdentifierType" -> text
          .verifying("UNSUPPORTED_CLIENT_ID_TYPE", clientType => supportedTypes.contains(clientType)),
        "clientIdentifier" -> normalizedText.verifying(validateClientId),
        "knownFact"        -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientType, service, clientIdType, clientId, knownFact)
      })({ authorisationRequest =>
        Some(
          (
            authorisationRequest.clientType,
            authorisationRequest.service,
            authorisationRequest.clientIdentifierType,
            authorisationRequest.clientIdentifier,
            authorisationRequest.knownFact))
      }))

  object ClientForMtdItWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[String] = arg match {
      case (UserInputNinoAndPostcode(`personal`, HMRCMTDIT, Some(clientIdentifier), _), featureFlags)
          if featureFlags.showKfcMtdIt =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientForPirWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputNinoAndPostcode(`personal`, HMRCPIR, Some(_), _), featureFlags)
          if featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientWithItsaOrPirFlagOff {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputNinoAndPostcode(`personal`, _, Some(_), _), featureFlags)
          if !featureFlags.showKfcMtdIt || !featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientForVatWithFlagOn {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[String] = arg match {
      case (UserInputVrnAndRegDate(_, HMRCMTDVAT, Some(clientIdentifier), _), featureFlags)
          if featureFlags.showKfcMtdVat =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientWithVatFlagOff {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputVrnAndRegDate(_, _, Some(_), _), featureFlags) if !featureFlags.showKfcMtdVat =>
        Some(())
      case _ => None
    }
  }

  object CurrentInvitationInputItsaReady {
    def unapply(arg: CurrentAuthorisationRequest)(
      implicit featureFlags: FeatureFlags): Option[FastTrackItsaInvitation] =
      arg match {
        case CurrentAuthorisationRequest(_, HMRCMTDIT, "ni", clientIdentifier, postcodeOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcMtdIt || postcodeOpt.exists(
              _.matches(postcodeRegex))) =>
          Some(
            FastTrackItsaInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcMtdIt) postcodeOpt.map(Postcode) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputPirReady {
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[FastTrackPirInvitation] =
      arg match {
        case CurrentAuthorisationRequest(_, HMRCPIR, "ni", clientIdentifier, dobOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcPersonalIncome || dobOpt.exists(
              DateFieldHelper.validateDate)) =>
          Some(
            FastTrackPirInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcPersonalIncome) dobOpt.map(DOB) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputVatReady {
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[FastTrackVatInvitation] =
      arg match {
        case CurrentAuthorisationRequest(clientType, HMRCMTDVAT, "vrn", clientIdentifier, vatRegDateOpt, _)
            if clientType.isDefined && Vrn.isValid(clientIdentifier) && (!featureFlags.showKfcMtdVat || vatRegDateOpt
              .exists(DateFieldHelper.validateDate)) =>
          Some(
            FastTrackVatInvitation(
              clientType,
              Vrn(clientIdentifier),
              if (featureFlags.showKfcMtdVat) vatRegDateOpt.map(VatRegDate) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputNeedsClientIdentifier {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case CurrentAuthorisationRequest(clientType, service, _, clientIdentifier, _, _) =>
          service match {
            case HMRCMTDVAT if !Vrn.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCMTDVAT, "vrn", "", None))
            case HMRCMTDIT if !Nino.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCMTDIT, "ni", "", None))
            case HMRCPIR if !Nino.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCPIR, "ni", "", None))
            case _ => None
          }
        case _ => None
      }
  }

  object CurrentInvitationInputFromFastTrackNeedsClientType {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case input: CurrentAuthorisationRequest if input.fromFastTrack && input.clientType.isEmpty => Some(input)
        case _                                                                                     => None
      }
  }

  object CurrentInvitationInputNeedsClientType {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case input: CurrentAuthorisationRequest if !input.fromFastTrack && input.clientType.isEmpty => Some(input)
        case _                                                                                      => None
      }
  }

  object CurrentInvitationInputNeedsKnownFact {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case CurrentAuthorisationRequest(clientType, HMRCMTDVAT, _, _, Some(vatRegDate), true)
            if supportedClientTypes.contains(clientType) && !DateFieldHelper.validateDate(vatRegDate) =>
          Some(currentAuthorisationRequest.copy(knownFact = None))

        case CurrentAuthorisationRequest(`personal`, HMRCMTDIT, _, _, Some(postcode), true)
            if !postcode.matches(postcodeRegex) =>
          Some(currentAuthorisationRequest.copy(knownFact = None))

        case CurrentAuthorisationRequest(`personal`, HMRCPIR, _, _, Some(dob), true)
            if !DateFieldHelper.validateDate(dob) =>
          Some(currentAuthorisationRequest.copy(knownFact = None))

        case CurrentAuthorisationRequest(clientType, service, _, _, None, true)
            if supportedClientTypes.contains(clientType) && Services.supportedServices.contains(service) =>
          Some(currentAuthorisationRequest)

        case _ => None
      }
  }

  object CurrentInvitationInputNeedService {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case CurrentAuthorisationRequest(clientType, service, _, _, _, _) if service.isEmpty =>
          //TODO Change when mandatory
          Some(currentAuthorisationRequest.copy(clientType = clientType))
        case _ => None
      }
  }

}
