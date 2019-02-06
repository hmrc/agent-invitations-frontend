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

import org.joda.time.LocalDate
import play.api.data.Form
import play.api.mvc.{Call, Request, Result}
import play.api.{Configuration, Logger}
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentInvitationControllerSupport._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.TaxIdentifier
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

abstract class BaseInvitationController(
  override val withVerifiedPasscode: PasscodeVerification,
  override val authConnector: AuthConnector,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  auditService: AuditService
)(
  implicit override val externalUrls: ExternalUrls,
  configuration: Configuration,
  featureFlags: FeatureFlags,
  messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends BaseController(withVerifiedPasscode, authConnector, featureFlags) {

  val agentServicesAccountUrl = s"${externalUrls.agentServicesAccountUrl}/agent-services-account"

  protected def handleGetClientType(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.fetchAndClear.flatMap { _ =>
        currentAuthorisationRequestCache.fetch.flatMap {
          case Some(data) if data.clientType.isEmpty && data.fromFastTrack => Ok(clientTypePage())
          case _                                                           => currentAuthorisationRequestCache.fetchAndClear.map(_ => Ok(clientTypePage()))
        }
      }
    }

  protected def handleSubmitClientType(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      ClientTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => Ok(clientTypePage(formWithErrors)),
          userInput => {
            val updateAggregate = currentAuthorisationRequestCache.fetch
              .map(_.getOrElse(CurrentAuthorisationRequest()))
              .map(_.copy(clientType = Some(userInput)))

            updateAggregate.flatMap(
              toUpdate =>
                currentAuthorisationRequestCache
                  .save(toUpdate)
                  .flatMap(_ => redirectBasedOnCurrentInputState(arn, toUpdate, isWhitelisted)))
          }
        )
    }

  protected def handleGetSelectServicePage(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      getSelectServicePage(isWhitelisted)
    }

  private def getSelectServicePage(
    isWhitelisted: Boolean,
    form: Form[String] = ServiceTypeForm.form,
    businessForm: Form[Confirmation] = agentConfirmationForm("error.business-service.required"))(
    implicit request: Request[_]): Future[Result] =
    journeyStateCache.fetch.flatMap {
      case Some(basket) if basket.requests.nonEmpty =>
        basket.clientType match {
          case "personal" =>
            Ok(selectServicePage(form, enabledPersonalServicesForInvitation(isWhitelisted), basketFlag = true))
          case "business" => {
            Ok(business_select_service(businessForm, basketFlag = true))
          }
          case _ => Redirect(clientTypeCall)
        }
      case _ =>
        currentAuthorisationRequestCache.fetch.flatMap {
          case Some(input) if input.clientType.nonEmpty =>
            input.clientType match {
              case Some("personal") =>
                Ok(selectServicePage(form, enabledPersonalServicesForInvitation(isWhitelisted), basketFlag = false))
              case Some("business") =>
                Ok(business_select_service(businessForm, basketFlag = false))
              case _ => Redirect(clientTypeCall)
            }
          case _ => Redirect(clientTypeCall)
        }
    }

  protected def handleSubmitSelectService(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
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
                toUpdate =>
                  currentAuthorisationRequestCache
                    .save(toUpdate)
                    .flatMap(_ =>
                      ifShouldShowService(toUpdate, featureFlags, isWhitelisted) {
                        redirectBasedOnCurrentInputState(arn, toUpdate, isWhitelisted)
                    }))
            }

            currentAuthorisationRequestCache.fetch.flatMap { cache =>
              cache.flatMap(_.clientType) match {
                case Some("personal") => updateSessionAndRedirect
                case Some("business") =>
                  if (serviceType == HMRCMTDVAT) {
                    updateSessionAndRedirect
                  } else {
                    Redirect(selectServiceCall)
                  }
                case _ => Redirect(clientTypeCall)
              }
            }
          }
        )
    }

  protected def handleSubmitSelectServiceBusiness(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentConfirmationForm("error.business-service.required")
        .bindFromRequest()
        .fold(
          formWithErrors => getSelectServicePage(isWhitelisted, businessForm = formWithErrors),
          data => {
            if (data.choice) {
              val updateAggregate = currentAuthorisationRequestCache.fetch
                .map(_.getOrElse(CurrentAuthorisationRequest()))
                .map(_.copy(service = HMRCMTDVAT))

              updateAggregate.flatMap(
                toUpdate =>
                  currentAuthorisationRequestCache
                    .save(toUpdate)
                    .flatMap(_ =>
                      ifShouldShowService(toUpdate, featureFlags, isWhitelisted) {
                        redirectBasedOnCurrentInputState(arn, toUpdate, isWhitelisted)
                    }))
            } else Future successful Redirect(clientTypeCall)
          }
        )
    }

  protected def handleShowIdentifyClient(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.map {
        case Some(inviteDetails) if inviteDetails.service.nonEmpty =>
          inviteDetails.service match {
            case HMRCMTDIT =>
              Ok(
                identify_client_itsa(
                  ItsaClientForm.form(featureFlags.showKfcMtdIt),
                  featureFlags.showKfcMtdIt,
                  inviteDetails.fromFastTrack,
                  submitIdentifyClientCall))

            case HMRCMTDVAT =>
              Ok(
                identify_client_vat(
                  VatClientForm.form(featureFlags.showKfcMtdVat),
                  featureFlags.showKfcMtdVat,
                  inviteDetails.fromFastTrack,
                  submitIdentifyClientCall))

            case HMRCPIR =>
              Ok(
                identify_client_irv(
                  IrvClientForm.form(featureFlags.showKfcPersonalIncome),
                  featureFlags.showKfcPersonalIncome,
                  inviteDetails.fromFastTrack,
                  submitIdentifyClientCall))

            case _ => Redirect(selectServiceCall)
          }
        case _ => Redirect(clientTypeCall)
      }
    }

  protected def handleSubmitIdentifyClient(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(car) =>
          car.service match {
            case HMRCMTDIT  => identifyItsaClient(arn, isWhitelisted)
            case HMRCMTDVAT => identifyVatClient(arn, isWhitelisted)
            case HMRCPIR    => identifyIrvClient(arn, isWhitelisted)
            case _          => Redirect(selectServiceCall)
          }
        case _ => Redirect(clientTypeCall)
      }
    }

  protected def handleShowConfirmClient(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, _) =>
      currentAuthorisationRequestCache.fetch.flatMap {
        case Some(data) =>
          (data.clientIdentifier, data.service) match {
            case (clientId, service) if clientId.nonEmpty =>
              invitationsService.getClientNameByService(clientId, service).flatMap { name =>
                Ok(showConfirmClientPage(name))
              }
            case _ => Redirect(identifyClientCall)
          }
        case _ => Redirect(clientTypeCall)
      }
    }

  private def identifyItsaClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    ItsaClientForm
      .form(featureFlags.showKfcMtdIt)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(identify_client_itsa(formWithErrors, featureFlags.showKfcMtdIt, true, submitIdentifyClientCall)),
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
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    VatClientForm
      .form(featureFlags.showKfcMtdVat)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(identify_client_vat(formWithErrors, featureFlags.showKfcMtdVat, true, submitIdentifyClientCall)),
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
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    IrvClientForm
      .form(featureFlags.showKfcPersonalIncome)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(identify_client_irv(formWithErrors, featureFlags.showKfcPersonalIncome, true, submitIdentifyClientCall)),
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
        Redirect(clientTypeCall)

      case CurrentInvitationInputNeedsKnownFact(_) =>
        Redirect(routes.AgentsFastTrackInvitationController.showKnownFact())

      case CurrentInvitationInputNeedsClientIdentifier(invitationNeedsClientIdentifier) =>
        invitationNeedsClientIdentifier.service match {
          case service if isSupportedWhitelistedService(service, isWhitelisted) =>
            Redirect(identifyClientCall)
          case _ =>
            Redirect(clientTypeCall)
        }

      case CurrentInvitationInputFromFastTrackNeedsClientType(_) =>
        Redirect(clientTypeCall)

      case CurrentInvitationInputNeedsService(_) =>
        Redirect(selectServiceCall)

      case _ =>
        Logger(getClass).warn("Resetting due to mix data in session")
        currentAuthorisationRequestCache
          .save(CurrentAuthorisationRequest())
          .map(_ => Redirect(clientTypeCall))
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
                           "",
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
                           "",
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
            Redirect(confirmClientCall)
          case HMRCMTDVAT if featureFlags.enableMtdVatToConfirm =>
            Redirect(confirmClientCall)
          case HMRCPIR if featureFlags.enableIrvToConfirm =>
            Redirect(confirmClientCall)
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
                               toFuture(Redirect(routes.AgentsInvitationController.showReviewAuthorisations()))
                             case Some("business") => body
                             case _                => toFuture(Redirect(clientTypeCall))
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

  def clientTypeCall: Call = routes.AgentsInvitationController.showClientType()
  def clientTypePage(form: Form[String] = ClientTypeForm.form)(implicit request: Request[_]): Appendable =
    client_type(form, clientTypes, agentServicesAccountUrl)

  def selectServiceCall: Call = routes.AgentsInvitationController.showSelectService()
  def selectServicePage(
    form: Form[String] = ServiceTypeForm.form,
    enabledServices: Seq[(String, String)],
    basketFlag: Boolean)(implicit request: Request[_]): Appendable =
    select_service(form, enabledServices, basketFlag)

  def identifyClientCall: Call = routes.AgentsInvitationController.showIdentifyClient()
  def submitIdentifyClientCall: Call = routes.AgentsInvitationController.submitIdentifyClient()

  def confirmClientCall: Call = routes.AgentsInvitationController.showConfirmClient()
  def showConfirmClientPage(name: Option[String])(implicit request: Request[_]): Appendable =
    confirm_client(name.getOrElse(""), agentConfirmationForm("error.confirm-client.required"))
}
