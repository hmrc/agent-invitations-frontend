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
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{InvitationsConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.{VatInvitation, _}
import uk.gov.hmrc.agentinvitationsfrontend.repository.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{DeletePageConfig, InvitationSentPageConfig, ReviewAuthorisationsPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.Nino

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsInvitationController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  relationshipsConnector: RelationshipsConnector,
  auditService: AuditService,
  agentSessionCache: AgentSessionCache,
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
      agentSessionCache,
      relationshipsConnector,
      auditService
    ) {

  import AgentsInvitationController._

  private val invitationExpiryDuration = Duration(expiryDuration.replace('_', ' '))

  val agentsRootUrl: Call = routes.AgentsInvitationController.showClientType()

  val agentsRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(agentsRootUrl)
  }

  val showClientType: Action[AnyContent] = Action.async { implicit request =>
    handleGetClientType()
  }

  val submitClientType: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitClientType
  }

  val showSelectService: Action[AnyContent] = Action.async { implicit request =>
    handleGetSelectServicePage()
  }

  val submitSelectPersonalService: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitSelectServicePersonal
  }

  val submitSelectBusinessService: Action[AnyContent] = Action.async { implicit request =>
    handleSubmitSelectServiceBusiness(agentConfirmationForm("error.business-service.required"))
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
      agentSessionCache.fetch.flatMap {
        case Some(existingSession) =>
          val clientType = existingSession.clientType
          val clientId = existingSession.clientIdentifier.getOrElse("")
          val service = existingSession.service.getOrElse("")
          val knownFact = existingSession.knownFact
          val result: Future[Result] = {
            invitationsService.getClientNameByService(clientId, service).flatMap { name =>
              val clientName = name.getOrElse("")
              agentConfirmationForm("error.confirm-client.required")
                .bindFromRequest()
                .fold(
                  formWithErrors => Ok(confirm_client(clientName, formWithErrors, identifyClientCall.url)),
                  data =>
                    if (data.choice) {
                      maybeResultIfPendingInvitationsOrRelationshipExistFor(arn, clientId, service, existingSession)
                        .flatMap {
                          case Some(r) => r
                          case None =>
                            val updatedBasket = existingSession.requests ++ Seq(
                              AuthorisationRequest(clientName, Invitation(clientType, service, clientId, knownFact)))
                            agentSessionCache
                              .save(
                                AgentSession(
                                  clientType,
                                  requests = updatedBasket,
                                  clientTypeForInvitationSent = clientType))
                              .flatMap { _ =>
                                clientType match {
                                  case Some(ClientType.personal) =>
                                    Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
                                  case Some(ClientType.business) => confirmAndRedirect(arn, existingSession, false)
                                  case _                         => toFuture(Redirect(agentsRootUrl))
                                }
                              }
                        }
                    } else {
                      Redirect(routes.AgentsInvitationController.showIdentifyClient())
                  }
                )
            }
          }
          result
        case None => Redirect(agentsRootUrl)
      }
    }
  }

  def showReviewAuthorisations: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      for {
        sessionCache <- agentSessionCache.fetch
        result = sessionCache match {
          case Some(cache) if cache.requests.nonEmpty =>
            Ok(
              review_authorisations(
                ReviewAuthorisationsPageConfig(cache, featureFlags),
                agentConfirmationForm("error.review-authorisation.required"),
                backLinkForReviewAuthorisationsPage(cache.service.getOrElse(""))
              ))
          case Some(_) => Redirect(routes.AgentsInvitationController.allAuthorisationsRemoved())
          case None    => Redirect(agentsRootUrl)
        }
      } yield result
    }
  }

  def submitReviewAuthorisations: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      agentSessionCache.hardGet.flatMap { sessionCache =>
        agentConfirmationForm("error.review-authorisation.required")
          .bindFromRequest()
          .fold(
            formWithErrors => {
              Future.successful(
                Ok(
                  review_authorisations(
                    ReviewAuthorisationsPageConfig(sessionCache, featureFlags),
                    formWithErrors,
                    backLinkForReviewAuthorisationsPage(sessionCache.service.getOrElse("")))))
            },
            input => {
              if (input.choice) {
                Redirect(routes.AgentsInvitationController.showSelectService())
              } else {
                for {
                  processedRequests <- invitationsService
                                        .createMultipleInvitations(
                                          arn,
                                          sessionCache.clientType,
                                          sessionCache.requests,
                                          featureFlags)
                  _ <- agentSessionCache.save(sessionCache.copy(requests = processedRequests))
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
      agentSessionCache.hardGet.map { agentSession =>
        val deleteItem =
          agentSession.requests.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
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
            agentSessionCache.fetch.map {
              case Some(agentSession) =>
                val deleteItem =
                  agentSession.requests.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
                Ok(delete(DeletePageConfig(deleteItem), formWithErrors))
              case None => Redirect(agentsRootUrl)
            }
          },
          input =>
            if (input.choice) {
              agentSessionCache.fetch.flatMap {
                case Some(session) =>
                  agentSessionCache
                    .save(session.copy(requests = session.requests.filterNot(_.itemId == itemId)))
                    .flatMap(_ => Redirect(routes.AgentsInvitationController.showReviewAuthorisations()))
                case None => Redirect(routes.AgentsInvitationController.showClientType())
              }
            } else {
              Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
          }
        )
    }
  }

  val showInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      agentSessionCache.hardGet.flatMap { session =>
        val clientTypeForInvitationSent = session.clientTypeForInvitationSent.getOrElse(
          throw new IllegalStateException("no client type found in cache"))
        val continueUrlExists = session.continueUrl.isDefined
        invitationsService.createAgentLink(arn, Some(clientTypeForInvitationSent)).flatMap { agentLink =>
          val invitationUrl: String = s"${externalUrls.agentInvitationsExternalUrl}$agentLink"
          val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)
          //clear every thing in the cache except clientTypeForInvitationSent and continueUrl , as these needed in-case user refreshes the page
          agentSessionCache
            .save(
              AgentSession(
                clientTypeForInvitationSent = Some(clientTypeForInvitationSent),
                continueUrl = session.continueUrl))
            .map { _ =>
              Ok(
                invitation_sent(
                  InvitationSentPageConfig(
                    invitationUrl,
                    continueUrlExists,
                    featureFlags.enableTrackRequests,
                    ClientType.fromEnum(clientTypeForInvitationSent),
                    inferredExpiryDate)))
            }
        }
      }
    }
  }

  val continueAfterInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      agentSessionCache.hardGet.map { agentSession =>
        val continueUrl = agentSession.continueUrl.getOrElse(agentServicesAccountUrl)
        agentSessionCache.save(agentSession.copy(continueUrl = None))
        Redirect(continueUrl)
      }
    }
  }

  val notSignedUp: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      agentSessionCache.hardGet.flatMap { agentSession =>
        val hasRequests = agentSession.requests.nonEmpty
        agentSession.service match {
          case Some(HMRCMTDVAT) =>
            Forbidden(not_signed_up(Services.messageKeyForVAT, hasRequests))
          case Some(HMRCMTDIT) =>
            Forbidden(not_signed_up(Services.messageKeyForITSA, hasRequests))
          case ex =>
            throw new Exception(s"Unsupported Service: ${ex.getOrElse("")}")
        }
      }
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
        agentSession <- agentSessionCache.fetch.map {
                         case Some(cache) => cache
                         case None        => AgentSession()
                       }
      } yield {
        val backLinkUrl =
          if (agentSession.fromFastTrack)
            routes.AgentsFastTrackInvitationController.showKnownFact().url
          else routes.AgentsInvitationController.showConfirmClient().url
        Ok(pending_authorisation_exists(agentSession.requests.nonEmpty, backLinkUrl, agentSession.fromFastTrack))
      }
    }
  }

  private[controllers] def confirmAndRedirect(arn: Arn, agentSession: AgentSession, isWhitelisted: Boolean)(
    implicit request: Request[_]): Future[Result] = {
    val invitationEither: Either[Result, Invitation] = agentSession.service match {
      case Some(HMRCPIR) =>
        val kf = if (featureFlags.showKfcPersonalIncome) agentSession.knownFact.map(DOB(_)) else None
        Right(PirInvitation(Nino(agentSession.clientIdentifier.getOrElse("")), kf))

      case Some(HMRCMTDIT) =>
        val kf = if (featureFlags.showKfcMtdIt) agentSession.knownFact.map(Postcode(_)) else None
        Right(ItsaInvitation(Nino(agentSession.clientIdentifier.getOrElse("")), kf))

      case Some(HMRCMTDVAT) =>
        val kf = if (featureFlags.showKfcMtdVat) agentSession.knownFact.map(VatRegDate(_)) else None
        Right(VatInvitation(agentSession.clientType, Vrn(agentSession.clientIdentifier.getOrElse("")), kf))

      case e =>
        Logger.warn(s"unsupported service: $e, redirecting to /client-type")
        Left(Redirect(routes.AgentsInvitationController.showClientType()))
    }
    invitationEither match {
      case Left(result)      => result
      case Right(invitation) => createInvitation(arn, invitation)
    }
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
