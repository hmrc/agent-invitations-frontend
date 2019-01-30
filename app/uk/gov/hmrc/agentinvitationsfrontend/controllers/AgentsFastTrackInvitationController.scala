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
import play.api.data.Forms.{mapping, optional, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentAuthorisationRequest, FastTrackErrors, Services}
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.known_fact
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsFastTrackInvitationController @Inject()(
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  continueUrlCache: ContinueUrlCache,
  authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  withVerifiedPasscode: PasscodeVerification,
  auditService: AuditService,
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

  val agentFastTrackPostcodeForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackKnownFactForm(featureFlags, postcodeMapping(featureFlags.showKfcMtdIt))

  val agentFastTrackDateOfBirthForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackKnownFactForm(featureFlags, dateOfBirthMapping(featureFlags.showKfcPersonalIncome))

  val agentFastTrackVatRegDateForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackKnownFactForm(featureFlags, vatRegDateMapping(featureFlags))

  val agentInvitationIdentifyKnownFactForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackGenericFormKnownFact(featureFlags)

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

  private def withMaybeErrorUrlCached[A](
    block: Option[ContinueUrl] => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    continueUrlActions.withMaybeErrorUrl {
      case None      => block(None)
      case Some(url) => continueUrlCache.cacheAndFetchErrorUrl(url).flatMap(urlOps => block(urlOps))
    }

  private def withMaybeContinueUrlCached[A](
    block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    continueUrlActions.withMaybeContinueUrl {
      case None      => block
      case Some(url) => continueUrlCache.save(url).flatMap(_ => block)
    }
}

object AgentsFastTrackInvitationController {

  val validateFastTrackForm: Constraint[CurrentAuthorisationRequest] =
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

  def clientTypeFor(clientType: Option[String], service: String): Option[String] =
    clientType.orElse(service match {
      case "HMRC-MTD-IT"            => Some("personal")
      case "PERSONAL-INCOME-RECORD" => Some("personal")
      case _                        => None
    })

  val serviceNameForm: Form[String] = Form(
    mapping("service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)))(
      identity)(Some(_)))
}
