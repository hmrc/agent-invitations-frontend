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

import play.api.data.{Form, Mapping}
import play.api.data.Forms.{boolean, mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models.{CurrentAuthorisationRequest, FastTrackErrors}
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.CheckDetailsPageConfig
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{check_details, known_fact}
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsFastTrackInvitationController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  continueUrlCache: ContinueUrlCache,
  authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  withVerifiedPasscode: PasscodeVerification,
  auditService: AuditService)(
  implicit configuration: Configuration,
  externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends BaseInvitationController(
      withVerifiedPasscode,
      authConnector,
      invitationsService,
      invitationsConnector,
      relationshipsService,
      journeyStateCache,
      currentAuthorisationRequestCache,
      auditService) {

  val agentFastTrackPostcodeForm: Form[Option[String]] =
    knownFactsForm(postcodeMapping(featureFlags.showKfcMtdIt))

  val agentFastTrackDateOfBirthForm: Form[Option[String]] =
    knownFactsForm(dateOfBirthMapping(featureFlags.showKfcPersonalIncome))

  val agentFastTrackVatRegDateForm: Form[Option[String]] =
    knownFactsForm(vatRegDateMapping(featureFlags))

  val showKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      currentAuthorisationRequestCache.fetch.map {
        case Some(currentInvitation)
            if currentInvitation.clientIdentifier.nonEmpty && currentInvitation.clientIdentifierType.nonEmpty && currentInvitation.service.nonEmpty =>
          Ok(
            known_fact(
              getKnownFactFormForService(currentInvitation.service),
              currentInvitation.service,
              serviceToMessageKey(currentInvitation.service)))

        case Some(_) => throw new Exception("no content in cache")
        case None    => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      currentAuthorisationRequestCache.get.flatMap(
        currentAuthorisationRequest => {
          getKnownFactFormForService(currentAuthorisationRequest.service)
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Ok(
                  known_fact(
                    formWithErrors,
                    currentAuthorisationRequest.service,
                    serviceToMessageKey(currentAuthorisationRequest.service))),
              knownFact =>
                maybeResultIfPendingInvitationsOrRelationshipExistFor(
                  arn,
                  currentAuthorisationRequest.clientIdentifier,
                  currentAuthorisationRequest.service)
                  .flatMap {
                    case Some(r) => r
                    case None =>
                      val updatedCache = currentAuthorisationRequest.copy(knownFact = knownFact, fromFastTrack = true)
                      currentAuthorisationRequestCache
                        .save(updatedCache)
                        .flatMap(_ => redirectBasedOnCurrentInputState(arn, updatedCache, isWhitelisted))
                }
            )
        }
      )
    }
  }

  private def getKnownFactFormForService(service: String) =
    service match {
      case HMRCMTDIT  => agentFastTrackPostcodeForm
      case HMRCPIR    => agentFastTrackDateOfBirthForm
      case HMRCMTDVAT => agentFastTrackVatRegDateForm
      case p          => throw new Exception(s"invalid service in the cache during fast track journey: $p")
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
                    Future successful Redirect(routes.AgentsFastTrackInvitationController.showCheckDetails())
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

  def knownFactsForm(knownFactsMapping: Mapping[Option[String]]) =
    Form(single("knownFact" -> knownFactsMapping))

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

  val checkDetailsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("checkDetails" -> optional(boolean)
      .verifying(detailsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))
}
