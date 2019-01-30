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
import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional, text}
import play.api.mvc._
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{CheckDetailsPageConfig, DeletePageConfig, InvitationSentPageConfig, ReviewAuthorisationsPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl

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
  val env: Environment,
  authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  withVerifiedPasscode: PasscodeVerification,
  ecp: Provider[ExecutionContext])(
  implicit configuration: Configuration,
  externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  messagesApi: play.api.i18n.MessagesApi)
    extends BaseInvitationController(
      withVerifiedPasscode,
      authConnector,
      invitationsService,
      relationshipsService,
      journeyStateCache,
      currentAuthorisationRequestCache,
      auditService) {

  implicit val ec: ExecutionContext = ecp.get

  import AgentInvitationControllerSupport._
  import AgentsInvitationController._

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
          case "personal" =>
            Ok(select_service(form, enabledPersonalServicesForInvitation(isWhitelisted), true, "personal"))
          case "business" => Ok(select_service(form, vat ++ niOrg, false, "business"))
          case _          => Redirect(routes.AgentsInvitationController.showClientType())
        }
      case _ =>
        currentAuthorisationRequestCache.fetch.flatMap {
          case Some(input) if input.clientType.nonEmpty =>
            input.clientType match {
              case Some("personal") =>
                Ok(select_service(form, enabledPersonalServicesForInvitation(isWhitelisted), false, "personal"))
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

            currentAuthorisationRequestCache.fetch.flatMap { cache =>
              cache.flatMap(_.clientType) match {
                case Some("personal") => updateSessionAndRedirect
                case Some("business") =>
                  if (serviceType == HMRCMTDVAT || serviceType == HMRCNIORG) {
                    updateSessionAndRedirect
                  } else {
                    Redirect(routes.AgentsInvitationController.showSelectService())
                  }
                case _ => Redirect(routes.AgentsInvitationController.showClientType())
              }
            }
          }
        )
    }
  }

  val showIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.map {
        case Some(inviteDetails) if inviteDetails.service.nonEmpty =>
          inviteDetails.service match {
            case HMRCMTDIT =>
              Ok(
                identify_client_itsa(
                  ItsaClientForm.form(featureFlags.showKfcMtdIt),
                  featureFlags.showKfcMtdIt,
                  inviteDetails.fromFastTrack))

            case HMRCMTDVAT =>
              Ok(
                identify_client_vat(
                  VatClientForm.form(featureFlags.showKfcMtdVat),
                  featureFlags.showKfcMtdVat,
                  inviteDetails.fromFastTrack))

            case HMRCPIR =>
              Ok(
                identify_client_irv(
                  IrvClientForm.form(featureFlags.showKfcPersonalIncome),
                  featureFlags.showKfcPersonalIncome,
                  inviteDetails.fromFastTrack))

            case HMRCNIORG =>
              Ok(
                identify_client_niorg(
                  NiOrgClientForm.form(featureFlags.showHmrcNiOrg),
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
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(car) =>
          car.service match {
            case HMRCMTDIT  => identifyItsaClient(arn, isWhitelisted)
            case HMRCMTDVAT => identifyVatClient(arn, isWhitelisted)
            case HMRCPIR    => identifyIrvClient(arn, isWhitelisted)
            case HMRCNIORG  => identifyNiOrgClient(arn, isWhitelisted)
            case _          => Redirect(routes.AgentsInvitationController.showSelectService())
          }
        case _ => Redirect(routes.AgentsInvitationController.showClientType())
      }
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
        case Some(
            cachedItem @ CurrentInvitationInputWithNonEmptyClientId((clientType, clientId, service, knownFact))) =>
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
                                             currentCache.requests ++ Seq(AuthorisationRequest(
                                               clientName,
                                               Invitation(clientType, service, clientId, knownFact)))
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

  def identifyItsaClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[Result] =
    ItsaClientForm
      .form(featureFlags.showKfcMtdIt)
      .bindFromRequest()
      .fold(
        formWithErrors => Ok(identify_client_itsa(formWithErrors, featureFlags.showKfcMtdIt, true)),
        userInput =>
          for {
            maybeCachedInvitation <- currentAuthorisationRequestCache.fetch
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentAuthorisationRequest())
              .copy(
                clientIdentifier = userInput.clientIdentifier,
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
    VatClientForm
      .form(featureFlags.showKfcMtdVat)
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
                clientIdentifier = userInput.clientIdentifier,
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
    IrvClientForm
      .form(featureFlags.showKfcPersonalIncome)
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
                clientIdentifier = userInput.clientIdentifier,
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
    NiOrgClientForm
      .form(featureFlags.showHmrcNiOrg)
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
                clientIdentifier = userInput.clientIdentifier,
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
}

object AgentsInvitationController {

  //Forms
  val clientTypeOnlyForm: Form[Option[String]] = Form(mapping("clientType" -> optional(text)
    .verifying("Unsupported Client Type", clientType => supportedClientTypes.contains(clientType)))(identity)(Some(_)))

  val checkDetailsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("checkDetails" -> optional(boolean)
      .verifying(detailsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  def agentConfirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))
}
