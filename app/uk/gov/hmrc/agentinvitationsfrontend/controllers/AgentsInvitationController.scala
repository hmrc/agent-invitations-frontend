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

import javax.inject.{Inject, Named, Singleton}

import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms.{mapping, optional}
import play.api.mvc._
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{DeletePageConfig, InvitationSentPageConfig, ReviewAuthorisationsPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsInvitationController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  auditService: AuditService,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  continueUrlCache: ContinueUrlCache,
  val env: Environment,
  authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  withVerifiedPasscode: PasscodeVerification)(
  implicit configuration: Configuration,
  externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends BaseInvitationController(
      withVerifiedPasscode,
      authConnector,
      invitationsService,
      invitationsConnector,
      relationshipsService,
      journeyStateCache,
      currentAuthorisationRequestCache,
      auditService
    ) {

  import AgentInvitationControllerSupport._
  import AgentsInvitationController._

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))

  val agentsRootUrl: Call = routes.AgentsInvitationController.showClientType()

  val agentsRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(agentsRootUrl)
  }

  val showClientType: Action[AnyContent] = Action.async { implicit request =>
    handleGetClientType
  }

  val submitClientType: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitClientType
  }

  val showSelectService: Action[AnyContent] = Action.async { implicit request =>
    handleGetSelectServicePage
  }

  val submitSelectService: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitSelectService
  }

  val showIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    handleShowIdentifyClient
  }

  val submitIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitIdentifyClient
  }

  val showConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    handleShowConfirmClient
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
                    Ok(confirm_client(clientName, formWithErrors))
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
                                                    toFuture(Redirect(
                                                      routes.AgentsInvitationController.showReviewAuthorisations()))
                                                  else if (clientType == business) {
                                                    confirmAndRedirect(arn, cachedItem, false)
                                                  } else
                                                    toFuture(Redirect(agentsRootUrl))
                                     } yield redirect
                          } yield result

                      }
                    } else {
                      for {
                        _      <- currentAuthorisationRequestCache.save(CurrentAuthorisationRequest(clientType, service))
                        result <- Redirect(routes.AgentsInvitationController.showIdentifyClient())
                      } yield result
                  }
                )
            }
          }
          result
        case Some(_) => Redirect(routes.AgentsInvitationController.showIdentifyClient())
        case None    => Redirect(agentsRootUrl)
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
                ReviewAuthorisationsPageConfig(journeyState, featureFlags),
                agentConfirmationForm("error.review-authorisation.required")))
          case Some(_) => Redirect(routes.AgentsInvitationController.allAuthorisationsRemoved())
          case None    => Redirect(agentsRootUrl)
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
                Ok(review_authorisations(ReviewAuthorisationsPageConfig(journeyState, featureFlags), formWithErrors))),
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
                             Redirect(routes.AgentsInvitationController.showInvitationSent())
                           else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                             Redirect(routes.AgentsErrorController.allCreateAuthorisationFailed())
                           else Redirect(routes.AgentsErrorController.someCreateAuthorisationFailed())
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
              case None => Redirect(agentsRootUrl)
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
              Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
          }
        )
    }
  }

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
          val invitationUrl: String = s"${externalUrls.agentInvitationsExternalUrl}$agentLink"
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
      Ok(all_authorisations_removed())
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

  def agentConfirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))
}
