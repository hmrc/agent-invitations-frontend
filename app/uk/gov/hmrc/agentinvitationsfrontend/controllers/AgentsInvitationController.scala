/*
 * Copyright 2019 HM Revenue & Customs
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

import com.google.inject.Provider
import javax.inject.{Inject, Named, Singleton}
import org.joda.time.LocalDate
import play.api.data.Forms.{boolean, mapping, of, optional, text}
import play.api.data.{Form, Mapping}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc._
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.DateFieldHelper.{dateFieldsMapping, validDobDateFormat, validVatDateFormat}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{CheckDetailsPageConfig, DeletePageConfig, InvitationSentPageConfig, ReviewAuthorisationsPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Utr, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import play.api.data.format.Formats._
import uk.gov.hmrc.agentinvitationsfrontend.forms.{ClientTypeForm, ServiceTypeForm}

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsInvitationController @Inject()(
  @Named("agent-invitations-frontend.external-url") externalUrl: String,
  @Named("agent-services-account-frontend.external-url") asAccUrl: String,
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  auditService: AuditService,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  continueUrlCache: ContinueUrlCache,
  val messagesApi: play.api.i18n.MessagesApi,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification,
  ecp: Provider[ExecutionContext])(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags)
    extends FrontendController with I18nSupport with AuthActions {

  implicit val ec: ExecutionContext = ecp.get

  import AgentInvitationControllerSupport._
  import continueUrlActions._
  import AgentsInvitationController._

  val agentInvitationIdentifyClientFormItsa: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationIdentifyClientFormItsa(featureFlags)

  val agentInvitationIdentifyClientFormIrv: Form[UserInputNinoAndDob] =
    AgentsInvitationController.agentInvitationIdentifyClientFormIrv(featureFlags)

  val agentInvitationIdentifyClientFormVat: Form[UserInputVrnAndRegDate] =
    AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)

  val agentInvitationIdentifyClientFormNiOrg: Form[UserInputUtrAndPostcode] =
    AgentsInvitationController.agentInvitationIdentifyClientFormNiOrg(featureFlags)

  val agentInvitationIdentifyKnownFactForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackGenericFormKnownFact(featureFlags)

  val agentInvitationPostCodeForm: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

  val agentFastTrackPostcodeForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackKnownFactForm(featureFlags, postcodeMapping(featureFlags))

  val agentFastTrackDateOfBirthForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackKnownFactForm(featureFlags, dateOfBirthMapping(featureFlags))

  val agentFastTrackVatRegDateForm: Form[CurrentAuthorisationRequest] =
    AgentsInvitationController.agentFastTrackKnownFactForm(featureFlags, vatRegDateMapping(featureFlags))

  private val personalIncomeRecord =
    if (featureFlags.showPersonalIncome)
      Seq(HMRCPIR -> Messages("personal-select-service.personal-income-viewer"))
    else Seq.empty

  private val mtdItId =
    if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("personal-select-service.itsa")) else Seq.empty

  private val vat =
    if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  private val niOrg =
    if (featureFlags.showHmrcNiOrg) Seq(HMRCNIORG -> Messages("select-service.niorg")) else Seq.empty

  private val personalOption = Seq("personal" -> Messages("client-type.personal"))
  private val businessOption = Seq("business" -> Messages("client-type.business"))
  private val clientTypes = personalOption ++ businessOption

  private def enabledServices(isWhitelisted: Boolean): Seq[(String, String)] =
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat ++ niOrg
    } else {
      mtdItId ++ vat ++ niOrg
    }

  private val serviceToMessageKey: String => String = {
    case HMRCMTDIT  => messageKeyForITSA
    case HMRCPIR    => messageKeyForAfi
    case HMRCMTDVAT => messageKeyForVAT
    case HMRCNIORG  => messageKeyForNiOrg
    case _          => "Service is missing"
  }

  private[controllers] val isDevEnv =
    if (env.mode.equals(Mode.Test)) false else configuration.getString("run.mode").forall(Mode.Dev.toString.equals)
  private[controllers] val agentServicesAccountUrl: String =
    if (isDevEnv) s"http://localhost:9401/agent-services-account" else "/agent-services-account"

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))

  val agentsRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.AgentsInvitationController.showClientType())
  }

  val showClientType: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.fetchAndClear
      currentAuthorisationRequestCache.fetch.map {
        case Some(data) if data.clientType.isEmpty && data.fromFastTrack =>
          Ok(client_type(ClientTypeForm.form, clientTypes, agentServicesAccountUrl))
        case _ =>
          currentAuthorisationRequestCache.fetchAndClear
          Ok(client_type(ClientTypeForm.form, clientTypes, agentServicesAccountUrl))
      }
    }
  }

  val submitClientType: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      ClientTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => Future successful Ok(client_type(formWithErrors, clientTypes, agentServicesAccountUrl)),
          userInput => {
            val updateAggregate = currentAuthorisationRequestCache.fetch
              .map(_.getOrElse(CurrentAuthorisationRequest()))
              .map(_.copy(clientType = Some(userInput)))

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
      getSelectServicePage(isWhitelisted)
    }
  }

  private def getSelectServicePage(isWhitelisted: Boolean, form: Form[String] = ServiceTypeForm.form)(
    implicit request: Request[_]): Future[Result] =
    journeyStateCache.fetch.flatMap {
      case Some(basket) if basket.requests.nonEmpty =>
        basket.clientType match {
          case "personal" => Ok(select_service(form, enabledServices(isWhitelisted), true, "personal"))
          case "business" => Ok(select_service(form, vat ++ niOrg, false, "business"))
          case _          => Redirect(routes.AgentsInvitationController.showClientType())
        }
      case _ =>
        currentAuthorisationRequestCache.fetch.flatMap {
          case Some(input) if input.clientType.nonEmpty =>
            input.clientType match {
              case Some("personal") =>
                Ok(select_service(form, enabledServices(isWhitelisted), false, "personal"))
              case Some("business") =>
                Ok(select_service(form, vat ++ niOrg, false, "business"))
              case _ => Redirect(routes.AgentsInvitationController.showClientType())
            }
          case _ => Redirect(routes.AgentsInvitationController.showClientType())
        }
    }

  val submitSelectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      ServiceTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => getSelectServicePage(isWhitelisted, formWithErrors),
          serviceType => {

            def updateSessionAndRedirect = {
              val updateAggregate = currentAuthorisationRequestCache.fetch
                .map(_.getOrElse(CurrentAuthorisationRequest()))
                .map(_.copy(service = serviceType))

              updateAggregate.flatMap(
                updateFastTrack =>
                  currentAuthorisationRequestCache
                    .save(updateFastTrack)
                    .flatMap(_ =>
                      ifShouldShowService(updateFastTrack, featureFlags, isWhitelisted) {
                        redirectBasedOnCurrentInputState(arn, updateFastTrack, isWhitelisted)
                    }))
            }

            currentAuthorisationRequestCache.fetch.flatMap {
              cache =>
                cache.flatMap(_.clientType) match {
                  case Some("personal") => updateSessionAndRedirect
                  case Some("business") =>
                    if (serviceType == HMRCMTDVAT || serviceType == HMRCNIORG) {
                      updateSessionAndRedirect
                    } else {
                      for {
                        _      <- currentAuthorisationRequestCache.save(CurrentAuthorisationRequest())
                        result <- Redirect(routes.AgentsInvitationController.showSelectService())
                      } yield result
                    }
                  case _ => Redirect(routes.AgentsInvitationController.showClientType())
                }
            }
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

  private val authorisationRequestToIdentifyClientFormNiOrg = (authorisationRequest: CurrentAuthorisationRequest) => {
    val service = authorisationRequest.service
    val clientId = authorisationRequest.clientIdentifier
    agentInvitationIdentifyClientFormNiOrg.fill(
      UserInputUtrAndPostcode(authorisationRequest.clientType, service, Some(clientId), authorisationRequest.knownFact))
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

            case HMRCNIORG =>
              Ok(
                identify_client_niorg(
                  authorisationRequestToIdentifyClientFormNiOrg(inviteDetails),
                  featureFlags.showHmrcNiOrg,
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
            case HMRCNIORG  => identifyNiOrgClient(arn, isWhitelisted)
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
              cachedCurrentInvitationInput.flatMap(
                cacheItem =>
                  maybeResultIfPendingInvitationsOrRelationshipExistFor(
                    arn,
                    cacheItem.clientIdentifier,
                    cacheItem.service).flatMap {
                    case Some(r) => r
                    case None =>
                      cachedCurrentInvitationInput.flatMap { cii =>
                        redirectBasedOnCurrentInputState(arn, cii, isWhitelisted)
                      }
                })
            } else Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          }
        )
    }
  }

  def invitationExistsInBasket(service: String, clientId: String)(implicit hc: HeaderCarrier): Future[Boolean] =
    for {
      hasPendingInvitationServiceInJourney <- journeyStateCache.fetch.map {
                                               case Some(cache)
                                                   if cache.requests.map(_.service).contains(service) && cache.requests
                                                     .map(_.clientId)
                                                     .contains(clientId) =>
                                                 true
                                               case _ => false
                                             }
    } yield hasPendingInvitationServiceInJourney

  val showConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
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
        case _ => Future successful Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(cachedItem @ CurrentInvitationInputWithNonEmptyClientId((clientType, clientId, service))) =>
          val result: Future[Result] = {
            invitationsService.getClientNameByService(clientId, service).flatMap { name =>
              val clientName = name.getOrElse("")
              agentConfirmationForm("error.confirm-client.required")
                .bindFromRequest()
                .fold(
                  formWithErrors => {
                    Future successful Ok(confirm_client(clientName, formWithErrors))
                  },
                  data =>
                    if (data.choice) {
                      maybeResultIfPendingInvitationsOrRelationshipExistFor(arn, clientId, service).flatMap {
                        case Some(r) => Future.successful(r)
                        case None =>
                          for {
                            journeyStateOpt <- journeyStateCache.fetch
                            currentCache = journeyStateOpt match {
                              case None               => AgentMultiAuthorisationJourneyState("", Set.empty)
                              case Some(journeyState) => journeyState
                            }
                            result <- for {
                                       _ <- journeyStateCache.save(AgentMultiAuthorisationJourneyState(
                                             if (currentCache.clientType.nonEmpty) currentCache.clientType
                                             else clientType.getOrElse(""),
                                             currentCache.requests ++ Seq(
                                               AuthorisationRequest(clientName, clientType, service, clientId))
                                           ))
                                       redirect <- if (clientType == personal || currentCache.clientType == "personal")
                                                    Future successful Redirect(
                                                      routes.AgentsInvitationController.showReviewAuthorisations())
                                                  else if (clientType == business) {
                                                    confirmAndRedirect(arn, cachedItem, false)
                                                  } else
                                                    Future successful Redirect(
                                                      routes.AgentsInvitationController.showClientType())
                                     } yield redirect
                          } yield result

                      }
                    } else {
                      for {
                        _      <- currentAuthorisationRequestCache.save(CurrentAuthorisationRequest(clientType, service))
                        result <- Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
                      } yield result
                  }
                )
            }
          }
          result
        case Some(_) => Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
        case None    => Future successful Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  def maybeResultIfPendingInvitationsOrRelationshipExistFor(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier): Future[Option[Result]] =
    for {
      existsInBasket <- invitationExistsInBasket(service, clientId)
      hasPendingInvitations <- if (existsInBasket) Future.successful(true)
                              else
                                invitationsService.hasPendingInvitationsFor(arn, clientId, service)
      result <- hasPendingInvitations match {
                 case true =>
                   Future.successful(Some(Redirect(routes.AgentsInvitationController.pendingAuthorisationExists())))
                 case false =>
                   relationshipsService.hasActiveRelationshipFor(arn, clientId, service).map {
                     case true  => Some(Redirect(routes.AgentsErrorController.activeRelationshipExists()))
                     case false => None
                   }
               }
    } yield result

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
    withAuthorisedAsAgent { (arn, _) =>
      if (currentAuthorisationRequest.fromFastTrack) body
      else {
        currentAuthorisationRequest.service match {
          case HMRCMTDIT if featureFlags.enableMtdItToConfirm =>
            Redirect(routes.AgentsInvitationController.showConfirmClient())
          case HMRCMTDVAT if featureFlags.enableMtdVatToConfirm =>
            Redirect(routes.AgentsInvitationController.showConfirmClient())
          case HMRCPIR if featureFlags.enableIrvToConfirm =>
            Redirect(routes.AgentsInvitationController.showConfirmClient())
          case _ =>
            val result = for {
              existsInBasket <- invitationExistsInBasket(
                                 currentAuthorisationRequest.service,
                                 currentAuthorisationRequest.clientIdentifier)
              hasPendingInvitations <- if (existsInBasket) Future.successful(true)
                                      else
                                        invitationsService.hasPendingInvitationsFor(
                                          arn,
                                          currentAuthorisationRequest.clientIdentifier,
                                          currentAuthorisationRequest.service)
              hasActiveRelationship <- relationshipsService.hasActiveRelationshipFor(
                                        arn,
                                        currentAuthorisationRequest.clientIdentifier,
                                        currentAuthorisationRequest.service)
            } yield (hasPendingInvitations, hasActiveRelationship)

            result.flatMap {
              case (true, _)
                  if (currentAuthorisationRequest.service == Services.HMRCPIR && !featureFlags.enableIrvToConfirm) ||
                    (currentAuthorisationRequest.service == Services.HMRCMTDIT && !featureFlags.enableMtdItToConfirm) ||
                    (currentAuthorisationRequest.service == Services.HMRCMTDVAT && !featureFlags.enableMtdVatToConfirm) =>
                Future successful Redirect(routes.AgentsInvitationController.pendingAuthorisationExists())

              case (_, true)
                  if (currentAuthorisationRequest.service == Services.HMRCPIR && !featureFlags.enableIrvToConfirm) ||
                    (currentAuthorisationRequest.service == Services.HMRCMTDIT && !featureFlags.enableMtdItToConfirm) ||
                    (currentAuthorisationRequest.service == Services.HMRCMTDVAT && !featureFlags.enableMtdVatToConfirm) =>
                Future successful Redirect(routes.AgentsErrorController.activeRelationshipExists())
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
                              currentAuthorisationRequest.clientType,
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
          },
          data =>
            currentAuthorisationRequestCache.get.flatMap(cacheItem =>
              maybeResultIfPendingInvitationsOrRelationshipExistFor(arn, cacheItem.clientIdentifier, cacheItem.service)
                .flatMap {
                  case Some(r) => Future.successful(r)
                  case None => {
                    data match {
                      case Services.HMRCMTDIT =>
                        bindKnownFactForm(agentFastTrackPostcodeForm, arn, isWhitelisted, "itsa")
                      case Services.HMRCPIR =>
                        bindKnownFactForm(agentFastTrackDateOfBirthForm, arn, isWhitelisted, "afi")
                      case Services.HMRCMTDVAT =>
                        bindKnownFactForm(agentFastTrackVatRegDateForm, arn, isWhitelisted, "vat")
                    }
                  }
              })
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
            result <- for {
                       _ <- currentAuthorisationRequestCache.save(invitationWithClientDetails)
                       redirectResult <- redirectBasedOnCurrentInputState(
                                          arn,
                                          invitationWithClientDetails,
                                          isWhitelisted)
                     } yield redirectResult
          } yield result
      )

  def identifyNiOrgClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[Result] =
    agentInvitationIdentifyClientFormNiOrg
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(
            identify_client_niorg(
              formWithErrors,
              featureFlags.showHmrcNiOrg,
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
                clientIdentifierType = "utr",
                knownFact = userInput.postcode
              )
            result <- for {
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
      _ <- invitationsService.createInvitation(arn, fti, featureFlags)
    } yield Redirect(routes.AgentsInvitationController.showInvitationSent())

  val showInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      currentAuthorisationRequestCache.get.flatMap(cacheItem =>
        for {
          agentLink <- invitationsService.createAgentLink(
                        arn,
                        cacheItem.clientType.getOrElse(
                          throw new IllegalStateException("no client type found in cache")))
          continue <- continueUrlCache.fetch
        } yield {
          val invitationUrl: String = s"$externalUrl$agentLink"
          val clientType = if (agentLink.contains("personal")) "personal" else "business"
          val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)
          Ok(
            invitation_sent(
              InvitationSentPageConfig(
                invitationUrl,
                continue.isDefined,
                featureFlags.enableTrackRequests,
                clientType,
                inferredExpiryDate)))
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

  val notSignedUp: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.get.flatMap(aggregate =>
        journeyStateCache.get.map(cacheItem => {
          val hasRequests = cacheItem.requests.nonEmpty
          aggregate.service match {
            case HMRCMTDVAT =>
              Forbidden(not_signed_up(Services.messageKeyForVAT, hasRequests))
            case HMRCMTDIT =>
              Forbidden(not_signed_up(Services.messageKeyForITSA, hasRequests))
            case ex =>
              throw new Exception(s"Unsupported Service: $ex")
          }
        }))
    }
  }

  val allAuthorisationsRemoved: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(all_authorisations_removed())
    }
  }

  val pendingAuthorisationExists: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      for {
        cacheItem <- journeyStateCache.fetch.map {
                      case Some(cache) => cache
                      case None        => AgentMultiAuthorisationJourneyState("", Set.empty)
                    }
      } yield Ok(pending_authorisation_exists(cacheItem.requests.nonEmpty))
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
              Redirect(routes.AgentsInvitationController.notSignedUp())
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
                         Redirect(routes.AgentsInvitationController.notSignedUp())
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
      case HMRCNIORG if !featureFlags.showHmrcNiOrg =>
        Logger(getClass).warn(s"Service: $HMRCNIORG feature flagged is switched off")
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

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)

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

  //Validators
  private def validNino(
    nonEmptyFailure: String = "error.nino.required",
    invalidFailure: String = "enter-nino.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(nino => Nino.isValid(nino))

  private def validUtr(
    nonEmptyFailure: String = "error.utr.required",
    invalidFailure: String = "enter-utr.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(utr => Utr.isValid(utr))

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

  //Patterns
  private val postcodeCharactersRegex = "^[a-zA-Z0-9 ]+$"

  private[controllers] val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  def clientTypeFor(clientType: Option[String], service: String): Option[String] =
    clientType.orElse(service match {
      case "HMRC-MTD-IT"            => Some("personal")
      case "PERSONAL-INCOME-RECORD" => Some("personal")
      case _                        => None
    })

  //Constraints
  private val clientTypeChoice: Constraint[Option[String]] =
    radioChoice("error.client-type.required")

  private val serviceChoice: Constraint[Option[String]] =
    radioChoice("error.service.required")

  val detailsChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError("error.confirmDetails.invalid"))
  }

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

  private def confirmationChoice(errorMessage: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError(errorMessage))
  }

  //Mappings
  def postcodeMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(
      featureFlags.showKfcMtdIt,
      trimmedUppercaseText.verifying(
        validPostcode(
          featureFlags.showKfcMtdIt,
          "enter-postcode.invalid-format",
          "error.postcode.required",
          "enter-postcode.invalid-characters"))
    )

  def dateOfBirthMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(featureFlags.showKfcPersonalIncome, dateFieldsMapping(validDobDateFormat))

  def vatRegDateMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))

  val trimmedUppercaseText: Mapping[String] = of[String].transform(_.trim.toUpperCase, identity)

  val lowerCaseText: Mapping[String] = of[String].transform(_.trim.toLowerCase, identity)

  //Forms
  val clientTypeOnlyForm: Form[Option[String]] = Form(mapping("clientType" -> optional(text)
    .verifying("Unsupported Client Type", clientType => supportedClientTypes.contains(clientType)))(identity)(Some(_)))

  val agentInvitationBusinessServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> optional(text).verifying(serviceChoice),
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, service, _, _) =>
        UserInputNinoAndPostcode(clientType, service.getOrElse(""), None, None)
      })({ user =>
        Some((user.clientType, Some(user.service), None, None))
      }))
  }

  val serviceNameForm: Form[String] = Form(
    mapping("service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)))(
      identity)(Some(_)))

  val checkDetailsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("checkDetails" -> optional(boolean)
      .verifying(detailsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  def agentInvitationIdentifyClientFormItsa(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validNino()),
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

  def agentInvitationIdentifyClientFormIrv(featureFlags: FeatureFlags): Form[UserInputNinoAndDob] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validNino()),
        "knownFact" -> optionalIf(
          featureFlags.showKfcPersonalIncome,
          dateFieldsMapping(validDobDateFormat)
        )
      )({ (clientType, service, clientIdentifier, dob) =>
        UserInputNinoAndDob(clientType, service, Some(clientIdentifier.trim.toUpperCase()), dob)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.dob))
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

  def agentInvitationIdentifyClientFormNiOrg(featureFlags: FeatureFlags): Form[UserInputUtrAndPostcode] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validUtr()),
        "knownFact" -> optionalIf(
          featureFlags.showHmrcNiOrg,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showHmrcNiOrg,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (clientType, service, clientIdentifier, postcode) =>
        UserInputUtrAndPostcode(clientType, service, Some(clientIdentifier.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

}
