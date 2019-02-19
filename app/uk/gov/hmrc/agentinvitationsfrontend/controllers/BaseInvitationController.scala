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
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Call, Request, Result}
import play.api.{Configuration, Logger}
import play.twirl.api.HtmlFormat.Appendable
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{InvitationsConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.forms._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.ClientTypePageConfig
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

abstract class BaseInvitationController(
  override val withVerifiedPasscode: PasscodeVerification,
  override val authConnector: AuthConnector,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  agentSessionCache: AgentSessionCache,
  relationshipsConnector: RelationshipsConnector,
  auditService: AuditService
)(
  implicit override val externalUrls: ExternalUrls,
  configuration: Configuration,
  featureFlags: FeatureFlags,
  messages: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  val personalIncomeRecord =
    if (featureFlags.showPersonalIncome)
      Seq(HMRCPIR -> Messages("personal-select-service.personal-income-viewer"))
    else Seq.empty

  val mtdItId =
    if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("personal-select-service.itsa")) else Seq.empty

  val vat =
    if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  def enabledPersonalServices(isWhitelisted: Boolean): Seq[(String, String)] =
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }

  val serviceToMessageKey: String => String = {
    case HMRCMTDIT  => messageKeyForITSA
    case HMRCPIR    => messageKeyForAfi
    case HMRCMTDVAT => messageKeyForVAT
    case p          => p
  }

  val agentServicesAccountUrl = s"${externalUrls.agentServicesAccountUrl}/agent-services-account"

  protected def handleGetClientType(
    isDeAuthJourney: Boolean = false)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, _) =>
      agentSessionCache.fetch.flatMap {
        case Some(cache) if cache.clientType.isEmpty && cache.fromFastTrack =>
          Ok(clientTypePage(backLinkUrl = routes.AgentsFastTrackInvitationController.showCheckDetails().url))
        case _ =>
          agentSessionCache.save(AgentSession(isDeAuthJourney = isDeAuthJourney)).map(_ => Ok(clientTypePage()))
      }
    }

  protected def handleSubmitClientType(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      ClientTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => Ok(clientTypePage(formWithErrors)),
          userInput => {
            agentSessionCache.fetch
              .flatMap {
                case Some(cache) =>
                  agentSessionCache.save(
                    cache.copy(clientType = Some(userInput), clientTypeForInvitationSent = Some(userInput)))
                case None =>
                  agentSessionCache.save(
                    AgentSession(clientType = Some(userInput), clientTypeForInvitationSent = Some(userInput)))
              }
              .flatMap { updatedSession =>
                if (updatedSession.fromFastTrack)
                  redirectFastTrackToNextPage(arn, updatedSession, isWhitelisted)
                else
                  Redirect(selectServiceCall)
              }
          }
        )
    }

  protected def handleGetSelectServicePage(
    businessForm: Form[Confirmation] = agentConfirmationForm("error.business-service.required"))(
    implicit hc: HeaderCarrier,
    request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      getSelectServicePage(isWhitelisted, businessForm = businessForm)
    }

  private def getSelectServicePage(
    isWhitelisted: Boolean,
    form: Form[String] = ServiceTypeForm.form,
    businessForm: Form[Confirmation])(implicit request: Request[_]): Future[Result] =
    agentSessionCache.fetch.flatMap {
      case Some(cache) =>
        cache.clientType match {
          case Some(ClientType.personal) =>
            Ok(selectServicePage(form, enabledPersonalServices(isWhitelisted), basketFlag = cache.requests.nonEmpty))
          case Some(ClientType.business) =>
            Ok(businessSelectServicePage(businessForm, basketFlag = cache.requests.nonEmpty, clientTypeCall.url))
          case _ => Redirect(clientTypeCall)
        }
      case _ => Redirect(clientTypeCall)
    }

  protected def handleSubmitSelectService(
    businessForm: Form[Confirmation] = agentConfirmationForm("error.business-service.required"))(
    implicit hc: HeaderCarrier,
    request: Request[_]): Future[Result] =
    agentSessionCache.fetch.flatMap { car =>
      car.flatMap(_.clientType) match {
        case Some(ClientType.personal) => handleSubmitSelectServicePersonal(businessForm)
        case Some(ClientType.business) => handleSubmitSelectServiceBusiness(businessForm)
        case _                         => Redirect(clientTypeCall)
      }
    }

  private def handleSubmitSelectServicePersonal(
    businessForm: Form[Confirmation])(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      ServiceTypeForm.form
        .bindFromRequest()
        .fold(
          formWithErrors => getSelectServicePage(isWhitelisted, formWithErrors, businessForm),
          serviceInput => {
            def updateSessionAndRedirect(agentSession: Option[AgentSession]): Future[Result] =
              agentSession match {
                case Some(cache) =>
                  agentSessionCache
                    .save(cache.copy(clientType = Some(ClientType.personal), service = Some(serviceInput)))
                    .flatMap(_ =>
                      ifShouldShowService(serviceInput, featureFlags, isWhitelisted) {
                        if (isSupportedWhitelistedService(serviceInput, isWhitelisted)) Redirect(identifyClientCall)
                        else Redirect(clientTypeCall)
                    })
                case None => Redirect(clientTypeCall)
              }

            agentSessionCache.fetch.flatMap { cache =>
              cache.flatMap(_.clientType) match {
                case Some(ClientType.personal) => updateSessionAndRedirect(cache)
                case Some(ClientType.business) =>
                  if (serviceInput == HMRCMTDVAT) {
                    updateSessionAndRedirect(cache)
                  } else {
                    Redirect(selectServiceCall)
                  }
                case _ => Redirect(clientTypeCall)
              }
            }
          }
        )
    }

  private def handleSubmitSelectServiceBusiness(
    businessForm: Form[Confirmation])(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      businessForm
        .bindFromRequest()
        .fold(
          formWithErrors => getSelectServicePage(isWhitelisted, businessForm = formWithErrors),
          data => {
            if (data.choice) {
              agentSessionCache.fetch.flatMap {
                case Some(cache) =>
                  agentSessionCache
                    .save(cache.copy(clientType = Some(ClientType.business), service = Some(HMRCMTDVAT)))
                    .flatMap(_ =>
                      ifShouldShowService(HMRCMTDVAT, featureFlags, isWhitelisted) {
                        if (isSupportedWhitelistedService(HMRCMTDVAT, isWhitelisted)) Redirect(identifyClientCall)
                        else Redirect(clientTypeCall)
                    })
                case None => Redirect(clientTypeCall)
              }

            } else Future successful Redirect(clientTypeCall)
          }
        )
    }

  protected def handleShowIdentifyClient(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (_, _) =>
      agentSessionCache.fetch.map {
        case Some(cache) =>
          cache.service match {
            case Some(HMRCMTDIT) =>
              Ok(
                identify_client_itsa(
                  ItsaClientForm.form(featureFlags.showKfcMtdIt),
                  featureFlags.showKfcMtdIt,
                  submitIdentifyClientCall,
                  selectServiceCall.url))

            case Some(HMRCMTDVAT) =>
              Ok(
                identify_client_vat(
                  VatClientForm.form(featureFlags.showKfcMtdVat),
                  featureFlags.showKfcMtdVat,
                  submitIdentifyClientCall,
                  selectServiceCall.url))

            case Some(HMRCPIR) =>
              Ok(
                identify_client_irv(
                  IrvClientForm.form(featureFlags.showKfcPersonalIncome),
                  featureFlags.showKfcPersonalIncome,
                  submitIdentifyClientCall,
                  selectServiceCall.url
                ))

            case _ => Redirect(selectServiceCall)
          }
        case _ => Redirect(clientTypeCall)
      }
    }

  protected def handleSubmitIdentifyClient(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentSessionCache.fetch.flatMap {
        case Some(session) =>
          session.service match {
            case Some(HMRCMTDIT)  => identifyItsaClient(arn, isWhitelisted)
            case Some(HMRCMTDVAT) => identifyVatClient(arn, isWhitelisted)
            case Some(HMRCPIR)    => identifyIrvClient(arn, isWhitelisted)
            case _                => Redirect(selectServiceCall)
          }
        case _ => Redirect(clientTypeCall)
      }
    }

  protected def handleShowConfirmClient(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, _) =>
      agentSessionCache.fetch.flatMap {
        case Some(cache) =>
          (cache.clientIdentifier, cache.service) match {
            case (Some(clientId), Some(service)) if clientId.nonEmpty =>
              invitationsService.getClientNameByService(clientId, service).flatMap { name =>
                Ok(showConfirmClientPage(name, identifyClientCall.url))
              }
            case _ => Redirect(identifyClientCall)
          }
        case _ => Redirect(clientTypeCall)
      }
    }

  def identifyItsaClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    ItsaClientForm
      .form(featureFlags.showKfcMtdIt)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            identify_client_itsa(
              formWithErrors,
              featureFlags.showKfcMtdIt,
              submitIdentifyClientCall,
              selectServiceCall.url)),
        userInput =>
          agentSessionCache.fetch.flatMap {
            case Some(cache) =>
              val updatedSession =
                cache.copy(
                  clientIdentifier = Some(userInput.clientIdentifier),
                  clientIdentifierType = Some("ni"),
                  knownFact = userInput.postcode)
              agentSessionCache
                .save(updatedSession)
                .flatMap { updatedSession =>
                  val itsaInvitation = ItsaInvitation(
                    Nino(userInput.clientIdentifier),
                    if (featureFlags.showKfcMtdIt) userInput.postcode.map(Postcode(_)) else None)
                  knownFactCheckItsa(arn, updatedSession, itsaInvitation, isWhitelisted)
                }
            case None => Redirect(clientTypeCall)
        }
      )

  def identifyVatClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    VatClientForm
      .form(featureFlags.showKfcMtdVat)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            identify_client_vat(
              formWithErrors,
              featureFlags.showKfcMtdVat,
              submitIdentifyClientCall,
              selectServiceCall.url)),
        userInput =>
          agentSessionCache.fetch.flatMap {
            case Some(cache) =>
              val updatedSession =
                cache.copy(
                  clientIdentifier = Some(userInput.clientIdentifier),
                  clientIdentifierType = Some("vrn"),
                  knownFact = userInput.registrationDate)
              agentSessionCache
                .save(updatedSession)
                .flatMap { updatedSession =>
                  val vatInvitation =
                    VatInvitation(
                      updatedSession.clientType,
                      Vrn(userInput.clientIdentifier),
                      if (featureFlags.showKfcMtdVat) userInput.registrationDate.map(VatRegDate(_)) else None)
                  knownFactCheckVat(arn, updatedSession, vatInvitation, isWhitelisted)
                }
            case None => Redirect(clientTypeCall)
        }
      )

  def identifyIrvClient(arn: Arn, isWhitelisted: Boolean)(
    implicit request: Request[_],
    hc: HeaderCarrier): Future[Result] =
    IrvClientForm
      .form(featureFlags.showKfcPersonalIncome)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            identify_client_irv(
              formWithErrors,
              featureFlags.showKfcPersonalIncome,
              submitIdentifyClientCall,
              selectServiceCall.url)),
        userInput =>
          agentSessionCache.fetch.flatMap {
            case Some(cache) =>
              val updatedSession =
                cache.copy(
                  clientIdentifier = Some(userInput.clientIdentifier),
                  clientIdentifierType = Some("ni"),
                  knownFact = userInput.dob)
              agentSessionCache
                .save(updatedSession)
                .flatMap { updatedSession =>
                  val pirInvitation =
                    PirInvitation(
                      Nino(userInput.clientIdentifier),
                      if (featureFlags.showKfcPersonalIncome) userInput.dob.map(DOB(_)) else None)
                  knownFactCheckIrv(arn, updatedSession, pirInvitation, isWhitelisted)
                }
            case None => Redirect(clientTypeCall)
        }
      )

  def ifShouldShowService(service: String, featureFlags: FeatureFlags, isWhitelisted: Boolean)(
    body: => Future[Result]): Future[Result] =
    service match {
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

  def maybeResultIfPendingInvitationsOrRelationshipExistFor(
    arn: Arn,
    clientId: String,
    service: String,
    agentSession: AgentSession)(implicit hc: HeaderCarrier): Future[Option[Result]] =
    for {
      hasPendingInvitations <- if (invitationExistsInBasket(service, clientId, agentSession)) Future.successful(true)
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

  def invitationExistsInBasket(service: String, clientId: String, agentSession: AgentSession): Boolean =
    agentSession.requests.map(_.invitation.service).contains(service) &&
      agentSession.requests.map(_.invitation.clientId).contains(clientId)

  private def isSupportedWhitelistedService(service: String, isWhitelisted: Boolean): Boolean =
    enabledPersonalServices(isWhitelisted).exists(_._1 == service)

  protected def knownFactCheckVat(
    arn: Arn,
    agentSession: AgentSession,
    vatInvitation: VatInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    vatInvitation.vatRegDate.map(date => LocalDate.parse(date.value)) match {
      case Some(vatRegDate) =>
        invitationsService
          .checkVatRegistrationDateMatches(vatInvitation.clientIdentifier, vatRegDate) flatMap {
          case Some(true) =>
            maybeResultIfPendingInvitationsOrRelationshipExistFor(
              arn,
              agentSession.clientIdentifier.getOrElse(""),
              agentSession.service.getOrElse(""),
              agentSession)
              .flatMap {
                case Some(r) if agentSession.fromFastTrack => r
                case _ =>
                  if (agentSession.isDeAuthJourney && !featureFlags.enableMtdVatToConfirm) {
                    checkRelationshipExistsForService(
                      arn,
                      agentSession.service.getOrElse(""),
                      agentSession.clientIdentifier.getOrElse("")).flatMap {
                      case true =>
                        redirectOrShowConfirmClient(agentSession, featureFlags) {
                          createInvitation(arn, vatInvitation)
                        }
                      case false =>
                        Redirect(routes.AgentsErrorController.notAuthorised())
                    }
                  } else {
                    redirectOrShowConfirmClient(agentSession, featureFlags) {
                      createInvitation(arn, vatInvitation)
                    }
                  }
              }

          case Some(false) =>
            Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: VAT Registration Date Does Not Match.")
            Redirect(notMatchedCall)
          case None =>
            Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: VAT Registration Not Found.")
            Redirect(routes.AgentsInvitationController.notSignedUp())
        }
      case None =>
        redirectOrShowConfirmClient(agentSession, featureFlags) {
          createInvitation(arn, vatInvitation)
        }
    }

  protected def knownFactCheckItsa(
    arn: Arn,
    agentSession: AgentSession,
    itsaInvitation: ItsaInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    itsaInvitation.postcode match {
      case Some(postcode) =>
        for {
          hasPostcode <- invitationsService
                          .checkPostcodeMatches(itsaInvitation.clientIdentifier, postcode.value)
          result <- hasPostcode match {
                     case Some(true) =>
                       maybeResultIfPendingInvitationsOrRelationshipExistFor(
                         arn,
                         agentSession.clientIdentifier.getOrElse(""),
                         agentSession.service.getOrElse(""),
                         agentSession)
                         .flatMap {
                           case Some(r) if agentSession.fromFastTrack => r
                           case _ =>
                             if (agentSession.isDeAuthJourney && !featureFlags.enableMtdItToConfirm) {
                               checkRelationshipExistsForService(
                                 arn,
                                 agentSession.service.getOrElse(""),
                                 agentSession.clientIdentifier.getOrElse("")).flatMap {
                                 case true =>
                                   redirectOrShowConfirmClient(agentSession, featureFlags) {
                                     createInvitation(arn, itsaInvitation)
                                   }
                                 case false =>
                                   Redirect(routes.AgentsErrorController.notAuthorised())
                               }
                             } else {
                               redirectOrShowConfirmClient(agentSession, featureFlags) {
                                 createInvitation(arn, itsaInvitation)
                               }
                             }
                         }
                     case Some(false) =>
                       Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
                       auditService.sendAgentInvitationSubmitted(
                         arn,
                         "",
                         itsaInvitation,
                         "",
                         "Fail",
                         Some("POSTCODE_DOES_NOT_MATCH"))
                       toFuture(Redirect(notMatchedCall))
                     case None =>
                       Logger(getClass).warn(
                         s"${arn.value}'s Invitation Creation Failed: Client Registration Not Found.")
                       auditService.sendAgentInvitationSubmitted(
                         arn,
                         "",
                         itsaInvitation,
                         "",
                         "Fail",
                         Some("CLIENT_REGISTRATION_NOT_FOUND"))
                       toFuture(Redirect(routes.AgentsInvitationController.notSignedUp()))
                   }
        } yield result
      case None =>
        redirectOrShowConfirmClient(agentSession, featureFlags) {
          createInvitation(arn, itsaInvitation)
        }
    }

  protected def knownFactCheckIrv(
    arn: Arn,
    agentSession: AgentSession,
    pirInvitation: PirInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    if (featureFlags.showKfcPersonalIncome) {
      pirInvitation.dob match {
        case Some(dob) =>
          invitationsService
            .checkCitizenRecordMatches(pirInvitation.clientIdentifier, LocalDate.parse(dob.value))
            .flatMap {
              case Some(true) =>
                maybeResultIfPendingInvitationsOrRelationshipExistFor(
                  arn,
                  agentSession.clientIdentifier.getOrElse(""),
                  agentSession.service.getOrElse(""),
                  agentSession)
                  .flatMap {
                    case Some(r) if agentSession.fromFastTrack => r
                    case _ =>
                      if (agentSession.isDeAuthJourney && !featureFlags.enableIrvToConfirm) {
                        checkRelationshipExistsForService(
                          arn,
                          agentSession.service.getOrElse(""),
                          agentSession.clientIdentifier.getOrElse("")).flatMap {
                          case true =>
                            redirectOrShowConfirmClient(agentSession, featureFlags) {
                              createInvitation(arn, pirInvitation)
                            }
                          case false =>
                            Redirect(routes.AgentsErrorController.notAuthorised())
                        }
                      } else {
                        redirectOrShowConfirmClient(agentSession, featureFlags) {
                          createInvitation(arn, pirInvitation)
                        }
                      }
                  }
              case Some(false) =>
                Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Not Matched from Citizen-Details.")
                Redirect(notMatchedCall)
              case None =>
                Logger(getClass).warn(
                  s"${arn.value}'s Invitation Creation Failed: No Record found from Citizen-Details.")
                Redirect(notMatchedCall)
            }
        case None =>
          Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: No KnownFact Provided")
          Redirect(notMatchedCall)
      }
    } else {
      redirectOrShowConfirmClient(agentSession, featureFlags) {
        createInvitation(arn, pirInvitation)
      }
    }

  def checkRelationshipExistsForService(arn: Arn, service: String, clientId: String)(
    implicit hc: HeaderCarrier): Future[Boolean] =
    service match {
      case HMRCMTDIT  => relationshipsConnector.checkItsaRelationship(arn, Nino(clientId))
      case HMRCPIR    => relationshipsService.checkPirRelationship(arn, Nino(clientId))
      case HMRCMTDVAT => relationshipsConnector.checkVatRelationship(arn, Vrn(clientId))
      case e => {
        throw new Error(s"Unsupported service for checking relationship: $e")
      }
    }

  def redirectOrShowConfirmClient(agentSession: AgentSession, featureFlags: FeatureFlags)(body: => Future[Result])(
    implicit request: Request[_]): Future[Result] =
    withAuthorisedAsAgent { (arn, _) =>
      if (agentSession.fromFastTrack) body
      else {
        val clientType = agentSession.clientType
        val service = agentSession.service.getOrElse(" ") //TODO: Fix these getOrElse calls
        val clientIdentifier = agentSession.clientIdentifier.getOrElse(" ")
        val knownFact = agentSession.knownFact

        service match {
          case HMRCMTDIT if featureFlags.enableMtdItToConfirm =>
            Redirect(confirmClientCall)
          case HMRCMTDVAT if featureFlags.enableMtdVatToConfirm =>
            Redirect(confirmClientCall)
          case HMRCPIR if featureFlags.enableIrvToConfirm =>
            Redirect(confirmClientCall)
          case _ =>
            val result = for {
              existsInBasket <- invitationExistsInBasket(service, clientIdentifier, agentSession)
              hasPendingInvitations <- if (existsInBasket) Future.successful(true)
                                      else
                                        invitationsService.hasPendingInvitationsFor(arn, clientIdentifier, service)
              hasActiveRelationship <- relationshipsService.hasActiveRelationshipFor(arn, clientIdentifier, service)
            } yield (hasPendingInvitations, hasActiveRelationship)

            val serviceEnabled = (service == HMRCPIR && !featureFlags.enableIrvToConfirm) ||
              (service == HMRCMTDIT && !featureFlags.enableMtdItToConfirm) ||
              (service == HMRCMTDVAT && !featureFlags.enableMtdVatToConfirm)

            result.flatMap {
              case (true, _) if serviceEnabled =>
                Redirect(routes.AgentsInvitationController.pendingAuthorisationExists())

              case (_, true) if serviceEnabled => Redirect(routes.AgentsErrorController.activeRelationshipExists())

              case _ =>
                invitationsService
                  .getClientNameByService(clientIdentifier, service)
                  .map { clientName =>
                    val updatedBasket = agentSession.requests ++ Set(
                      AuthorisationRequest(
                        clientName.getOrElse(""),
                        Invitation(clientType, service, clientIdentifier, knownFact)))
                    agentSessionCache.save(
                      AgentSession(
                        clientType = clientType,
                        requests = updatedBasket,
                        clientTypeForInvitationSent = clientType))
                  }
                  .flatMap { _ =>
                    clientType match {
                      case Some(ClientType.personal) =>
                        toFuture(Redirect(routes.AgentsInvitationController.showReviewAuthorisations()))
                      case Some(ClientType.business) => body
                      case _                         => toFuture(Redirect(clientTypeCall))
                    }
                  }
            }
        }
      }
    }

  protected def redirectFastTrackToNextPage(arn: Arn, agentSession: AgentSession, isWhitelisted: Boolean)(
    implicit hc: HeaderCarrier,
    request: Request[_]): Future[Result] =
    maybeResultIfPendingInvitationsOrRelationshipExistFor(
      arn,
      agentSession.clientIdentifier.getOrElse(""),
      agentSession.service.getOrElse(""),
      agentSession).flatMap {
      case Some(r) => r
      case None =>
        agentSession match {
          case AgentSession(Some(clientType), Some(service), _, Some(clientIdentifier), knownFact, _, _, _, _, _, _) =>
            val knownFactRequired = knownFact.isEmpty &&
              ((service == HMRCPIR && featureFlags.showKfcPersonalIncome)
                || (service == HMRCMTDVAT && featureFlags.showKfcMtdVat)
                || (service == HMRCMTDIT && featureFlags.showKfcMtdIt))

            if (knownFactRequired) {
              Redirect(routes.AgentsFastTrackInvitationController.showKnownFact())
            } else {
              service match {
                case HMRCPIR =>
                  val pirInvitation = PirInvitation(
                    Nino(clientIdentifier),
                    if (featureFlags.showKfcPersonalIncome) knownFact.map(DOB(_)) else None)
                  knownFactCheckIrv(arn, agentSession, pirInvitation, isWhitelisted)
                case HMRCMTDVAT =>
                  val vatInvitation = VatInvitation(
                    Some(clientType),
                    Vrn(clientIdentifier),
                    if (featureFlags.showKfcMtdVat) knownFact.map(VatRegDate(_)) else None)
                  knownFactCheckVat(arn, agentSession, vatInvitation, isWhitelisted)
                case HMRCMTDIT =>
                  val itsaInvitation =
                    ItsaInvitation(
                      Nino(clientIdentifier),
                      if (featureFlags.showKfcMtdIt) knownFact.map(Postcode(_)) else None)
                  knownFactCheckItsa(arn, agentSession, itsaInvitation, isWhitelisted)
                case _ =>
                  Redirect(routes.AgentsFastTrackInvitationController.showKnownFact())
              }
            }

          case session if session.clientType.isEmpty && session.service.contains(HMRCMTDVAT) =>
            Redirect(routes.AgentsInvitationController.showClientType())

          case _ =>
            Redirect(routes.AgentsFastTrackInvitationController.showKnownFact())
        }
    }

  private[controllers] def createInvitation[T <: TaxIdentifier](arn: Arn, invitation: Invitation)(
    implicit request: Request[_]) =
    for {
      _ <- invitationsService.createInvitation(arn, invitation, featureFlags)
    } yield Redirect(routes.AgentsInvitationController.showInvitationSent())

  def clientTypeCall: Call = routes.AgentsInvitationController.showClientType()

  def clientTypePage(form: Form[ClientType] = ClientTypeForm.form, backLinkUrl: String = agentServicesAccountUrl)(
    implicit request: Request[_]): Appendable =
    client_type(form, ClientTypePageConfig(Some(backLinkUrl)))

  def selectServiceCall: Call = routes.AgentsInvitationController.showSelectService()

  def submitServiceCall: Call = routes.AgentsInvitationController.submitSelectService()

  def selectServicePage(
    form: Form[String] = ServiceTypeForm.form,
    enabledServices: Seq[(String, String)],
    basketFlag: Boolean)(implicit request: Request[_]): Appendable =
    select_service(form, enabledServices, basketFlag)

  def businessSelectServicePage(form: Form[Confirmation], basketFlag: Boolean, backLinkUrl: String)(
    implicit request: Request[_]): Appendable =
    business_select_service(form, basketFlag, submitServiceCall, backLinkUrl)

  def identifyClientCall: Call = routes.AgentsInvitationController.showIdentifyClient()

  def submitIdentifyClientCall: Call = routes.AgentsInvitationController.submitIdentifyClient()

  def confirmClientCall: Call = routes.AgentsInvitationController.showConfirmClient()

  def notMatchedCall: Call = routes.AgentsErrorController.notMatched()

  def showConfirmClientPage(name: Option[String], backLinkUrl: String)(implicit request: Request[_]): Appendable =
    confirm_client(name.getOrElse(""), agentConfirmationForm("error.confirm-client.required"), backLinkUrl)

  protected def backLinkForConfirmCancelPage(service: String): String =
    if (service == HMRCPIR) identifyClientCall.url else confirmClientCall.url

  protected def backLinkForReviewAuthorisationsPage(service: String): String =
    if (service == HMRCPIR) identifyClientCall.url else confirmClientCall.url
}
