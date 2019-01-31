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
import javax.inject.{Inject, Singleton}
import org.joda.time.LocalDate
import play.api.mvc.{Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationControllerSupport._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCNIORG, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.TaxIdentifier
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

abstract class BaseInvitationController(
  override val withVerifiedPasscode: PasscodeVerification,
  override val authConnector: AuthConnector,
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  auditService: AuditService,
  ecp: Provider[ExecutionContext]
)(
  implicit override val externalUrls: ExternalUrls,
  configuration: Configuration,
  featureFlags: FeatureFlags,
  messagesApi: play.api.i18n.MessagesApi)
    extends BaseController(withVerifiedPasscode, authConnector, featureFlags, ecp) {

  def ifShouldShowService(
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

  def maybeResultIfPendingInvitationsOrRelationshipExistFor(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier): Future[Option[Result]] =
    for {
      existsInBasket <- invitationExistsInBasket(service, clientId)
      hasPendingInvitations <- if (existsInBasket) Future.successful(true)
                              else
                                invitationsService.hasPendingInvitationsFor(arn, clientId, service)
      result <- if (hasPendingInvitations) {
                 toFuture(Some(Redirect(routes.AgentsInvitationController.pendingAuthorisationExists())))
               } else {
                 relationshipsService.hasActiveRelationshipFor(arn, clientId, service).map {
                   case true  => Some(Redirect(routes.AgentsErrorController.activeRelationshipExists()))
                   case false => None
                 }
               }
    } yield result

  def invitationExistsInBasket(service: String, clientId: String)(implicit hc: HeaderCarrier): Future[Boolean] =
    for {
      hasPendingInvitationServiceInJourney <- journeyStateCache.fetch.map {
                                               case Some(cache)
                                                   if cache.requests
                                                     .map(_.invitation.service)
                                                     .contains(service) && cache.requests
                                                     .map(_.invitation.clientId)
                                                     .contains(clientId) =>
                                                 true
                                               case _ => false
                                             }
    } yield hasPendingInvitationServiceInJourney

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
        Future successful Redirect(routes.AgentsFastTrackInvitationController.showKnownFact())

      case CurrentInvitationInputNeedsClientIdentifier(invitationNeedsClientIdentifier) =>
        invitationNeedsClientIdentifier.service match {
          case service if isSupportedWhitelistedService(service, isWhitelisted) =>
            Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          case _ =>
            Future successful Redirect(routes.AgentsInvitationController.showClientType())
        }

      case CurrentInvitationInputFromFastTrackNeedsClientType(_) =>
        Future successful Redirect(routes.AgentsInvitationController.showClientType())

      case CurrentInvitationInputNeedsService(_) =>
        Future successful Redirect(routes.AgentsInvitationController.showSelectService())

      case _ =>
        Logger(getClass).warn("Resetting due to mix data in session")
        currentAuthorisationRequestCache
          .save(CurrentAuthorisationRequest())
          .map(_ => Redirect(routes.AgentsInvitationController.showClientType()))
    }

  private def isSupportedWhitelistedService(service: String, isWhitelisted: Boolean): Boolean =
    enabledPersonalServicesForInvitation(isWhitelisted).exists(_._1 == service)

  private def knownFactCheckVat(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    vatInvitation: VatInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    vatInvitation.vatRegDate.map(date => LocalDate.parse(date.value)) match {
      case Some(vatRegDate) =>
        invitationsService
          .checkVatRegistrationDateMatches(vatInvitation.clientIdentifier, vatRegDate) flatMap {
          case Some(true) =>
            redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
              createInvitation(arn, vatInvitation)
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
          createInvitation(arn, vatInvitation)
        }
    }

  private def knownFactCheckItsa(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    itsaInvitation: ItsaInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    itsaInvitation.postcode match {
      case Some(postcode) =>
        for {
          hasPostcode <- invitationsService
                          .checkPostcodeMatches(itsaInvitation.clientIdentifier, postcode.value)
          result <- hasPostcode match {
                     case Some(true) =>
                       redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
                         createInvitation(arn, itsaInvitation)
                       }
                     case Some(false) =>
                       currentAuthorisationRequestCache.save(currentAuthorisationRequest).map { _ =>
                         Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
                         auditService.sendAgentInvitationSubmitted(
                           arn,
                           "",
                           itsaInvitation,
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
                           itsaInvitation,
                           "Fail",
                           Some("CLIENT_REGISTRATION_NOT_FOUND"))
                         Redirect(routes.AgentsInvitationController.notSignedUp())
                       }
                   }
        } yield result
      case None =>
        redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
          createInvitation(arn, itsaInvitation)
        }
    }

  private def knownFactCheckIrv(
    arn: Arn,
    currentAuthorisationRequest: CurrentAuthorisationRequest,
    pirInvitation: PirInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]) =
    if (featureFlags.showKfcPersonalIncome) {
      pirInvitation.dob match {
        case Some(dob) =>
          invitationsService
            .checkCitizenRecordMatches(pirInvitation.clientIdentifier, LocalDate.parse(dob.value))
            .flatMap {
              case Some(true) =>
                redirectOrShowConfirmClient(currentAuthorisationRequest, featureFlags) {
                  createInvitation(arn, pirInvitation)
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
        createInvitation(arn, pirInvitation)
      }
    }

  def redirectOrShowConfirmClient(currentAuthorisationRequest: CurrentAuthorisationRequest, featureFlags: FeatureFlags)(
    body: => Future[Result])(implicit request: Request[_]): Future[Result] =
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
                          currentCache.requests ++ Set(AuthorisationRequest(
                            clientName.getOrElse(""),
                            Invitation(
                              currentAuthorisationRequest.clientType,
                              currentAuthorisationRequest.service,
                              currentAuthorisationRequest.clientIdentifier,
                              currentAuthorisationRequest.knownFact
                            )
                          ))
                        ))
                  result <- currentAuthorisationRequest.clientType match {
                             case Some("personal") =>
                               Future successful Redirect(routes.AgentsInvitationController.showReviewAuthorisations())
                             case Some("business") => body
                             case _                => Future.successful(Redirect(routes.AgentsInvitationController.showClientType()))
                           }
                } yield result
              }
            }
        }
      }
    }

  private[controllers] def createInvitation[T <: TaxIdentifier](arn: Arn, invitation: Invitation)(
    implicit request: Request[_]) =
    for {
      _ <- invitationsService.createInvitation(arn, invitation, featureFlags)
    } yield Redirect(routes.AgentsInvitationController.showInvitationSent())

}
