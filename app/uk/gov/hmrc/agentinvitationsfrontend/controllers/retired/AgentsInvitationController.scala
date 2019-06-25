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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.retired

import javax.inject.{Inject, Named, Singleton}
import org.joda.time.LocalDate
import play.api.data.Form
import play.api.data.Forms.{mapping, optional}
import play.api.mvc._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, InvitationsConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.{VatInvitation, _}
import uk.gov.hmrc.agentinvitationsfrontend.repository.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{DeletePageConfig, InvitationSentPageConfig, PendingAuthorisationExistsPageConfig, ReviewAuthorisationsPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
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
  asaConnector: AgentServicesAccountConnector,
  auditService: AuditService,
  agentSessionCache: AgentSessionCache,
  authActions: AuthActions)(
  implicit configuration: Configuration,
  externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends BaseInvitationController(
      authActions,
      invitationsService,
      invitationsConnector,
      relationshipsService,
      agentSessionCache,
      relationshipsConnector,
      auditService
    ) {

  import AgentsInvitationController._
  import authActions._

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

  val submitIdentifyClientItsa: Action[AnyContent] = Action.async { implicit request =>
    identifyItsaClient
  }

  val submitIdentifyClientIrv: Action[AnyContent] = Action.async { implicit request =>
    identifyIrvClient
  }

  val submitIdentifyClientVat: Action[AnyContent] = Action.async { implicit request =>
    identifyVatClient
  }

  val showConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    handleShowConfirmClient
  }

  val submitConfirmClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
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
                  formWithErrors =>
                    Ok(
                      confirm_client(
                        clientName,
                        formWithErrors,
                        identifyClientCall.url,
                        routes.AgentsInvitationController.submitConfirmClient())),
                  data =>
                    if (data.choice) {
                      maybeResultIfPendingInvitationsOrRelationshipExistFor(
                        agent.arn,
                        clientId,
                        service,
                        existingSession)
                        .flatMap {
                          case Some(r) => r
                          case None =>
                            val updatedBasket = existingSession.requests ++ Seq(
                              AuthorisationRequest(
                                clientName,
                                Invitation(clientType, service, clientId, knownFact.getOrElse(""))))
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
                                  case Some(ClientType.business) =>
                                    confirmAndRedirect(agent.arn, existingSession, false)
                                  case _ => toFuture(Redirect(agentsRootUrl))
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
    withAuthorisedAsAgent { _ =>
      for {
        sessionCache <- agentSessionCache.fetch
        result = sessionCache match {
          case Some(cache) if cache.requests.nonEmpty =>
            Ok(
              review_authorisations(
                ReviewAuthorisationsPageConfig(
                  cache.requests,
                  featureFlags,
                  routes.AgentsInvitationController.submitReviewAuthorisations()),
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
    withAuthorisedAsAgent { agent =>
      agentSessionCache.hardGet.flatMap { sessionCache =>
        agentConfirmationForm("error.review-authorisation.required")
          .bindFromRequest()
          .fold(
            formWithErrors => {
              Future.successful(Ok(review_authorisations(
                ReviewAuthorisationsPageConfig(
                  sessionCache.requests,
                  featureFlags,
                  routes.AgentsInvitationController.submitReviewAuthorisations()),
                formWithErrors,
                backLinkForReviewAuthorisationsPage(sessionCache.service.getOrElse(""))
              )))
            },
            input => {
              if (input.choice) {
                Redirect(routes.AgentsInvitationController.showSelectService())
              } else {
                for {
                  processedRequests <- invitationsService
                                        .createMultipleInvitations(
                                          agent.arn,
                                          sessionCache.clientType,
                                          sessionCache.requests)
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
    withAuthorisedAsAgent { _ =>
      agentSessionCache.hardGet.map { agentSession =>
        val deleteItem =
          agentSession.requests.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
        Ok(
          delete(
            DeletePageConfig(deleteItem, routes.AgentsInvitationController.submitDelete(itemId)),
            agentConfirmationForm("error.delete.radio")))
      }
    }
  }

  def submitDelete(itemId: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentConfirmationForm("error.delete.radio")
        .bindFromRequest()
        .fold(
          formWithErrors => {
            agentSessionCache.fetch.map {
              case Some(agentSession) =>
                val deleteItem =
                  agentSession.requests.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
                Ok(
                  delete(
                    DeletePageConfig(deleteItem, routes.AgentsInvitationController.submitDelete(itemId)),
                    formWithErrors))
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
    withAuthorisedAsAgent { agent =>
      agentSessionCache.hardGet.flatMap { session =>
        val clientTypeForInvitationSent = session.clientTypeForInvitationSent.getOrElse(
          throw new IllegalStateException("no client type found in cache"))
        val continueUrlExists = session.continueUrl.isDefined
        invitationsService.createAgentLink(agent.arn, Some(clientTypeForInvitationSent)).flatMap { agentLink =>
          val inferredExpiryDate = LocalDate.now().plusDays(invitationExpiryDuration.toDays.toInt)
          //clear every thing in the cache except clientTypeForInvitationSent and continueUrl , as these needed in-case user refreshes the page
          agentSessionCache
            .save(
              AgentSession(
                clientTypeForInvitationSent = Some(clientTypeForInvitationSent),
                continueUrl = session.continueUrl))
            .flatMap { _ =>
              asaConnector.getAgencyEmail
                .flatMap(email =>
                  Ok(invitation_sent(InvitationSentPageConfig(
                    agentLink,
                    session.continueUrl,
                    continueUrlExists,
                    featureFlags.enableTrackRequests,
                    ClientType.fromEnum(clientTypeForInvitationSent),
                    inferredExpiryDate,
                    email
                  ))))
            }
        }
      }
    }
  }

  val continueAfterInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      for {
        agentSession <- agentSessionCache.hardGet
        continueUrl = agentSession.continueUrl.getOrElse(agentServicesAccountUrl)
        _ <- agentSessionCache.save(agentSession.copy(continueUrl = None))
      } yield Redirect(continueUrl)
    }
  }

  val notSignedUp: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.hardGet.flatMap { agentSession =>
        val hasRequests = agentSession.requests.nonEmpty
        agentSession.service match {
          case Some(HMRCMTDVAT) =>
            Forbidden(not_signed_up(HMRCMTDVAT, hasRequests))
          case Some(HMRCMTDIT) =>
            Forbidden(not_signed_up(HMRCMTDIT, hasRequests))
          case ex =>
            throw new Exception(s"Unsupported Service: ${ex.getOrElse("")}")
        }
      }
    }
  }

  val allAuthorisationsRemoved: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      Ok(all_authorisations_removed(routes.AgentsInvitationController.showClientType()))
    }
  }

  val pendingAuthorisationExists: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
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
        Ok(
          pending_authorisation_exists(PendingAuthorisationExistsPageConfig(
            agentSession.requests.nonEmpty,
            backLinkUrl,
            agentSession.fromFastTrack,
            featureFlags.enableTrackRequests,
            routes.AgentsInvitationController.showReviewAuthorisations(),
            routes.AgentsInvitationController.showClientType()
          )))
      }
    }
  }

  private[controllers] def confirmAndRedirect(arn: Arn, agentSession: AgentSession, isWhitelisted: Boolean)(
    implicit request: Request[_]): Future[Result] = {
    val invitationEither: Either[Result, Invitation] = agentSession.service match {
      case Some(HMRCPIR) =>
        val kf = agentSession.knownFact.map(DOB(_))
        Right(PirInvitation(Nino(agentSession.clientIdentifier.getOrElse("")), kf.getOrElse(DOB(""))))

      case Some(HMRCMTDIT) =>
        val kf = agentSession.knownFact.map(Postcode(_))
        Right(ItsaInvitation(Nino(agentSession.clientIdentifier.getOrElse("")), kf.getOrElse(Postcode(""))))

      case Some(HMRCMTDVAT) =>
        val kf = agentSession.knownFact.map(VatRegDate(_))
        Right(
          VatInvitation(
            agentSession.clientType,
            Vrn(agentSession.clientIdentifier.getOrElse("")),
            kf.getOrElse(VatRegDate(""))))

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
