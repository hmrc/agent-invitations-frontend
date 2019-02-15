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

import javax.inject.{Inject, Singleton}

import play.api.data.Forms.{boolean, mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{InvitationsConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsFastTrackInvitationController._
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.CheckDetailsPageConfig
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{check_details, known_fact}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
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
  relationshipsConnector: RelationshipsConnector,
  agentSessionCache: AgentSessionCache,
  authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  withVerifiedPasscode: PasscodeVerification,
  auditService: AuditService)(
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

  val agentFastTrackRoot = routes.AgentsFastTrackInvitationController.agentFastTrack()

  val agentFastTrackPostcodeForm: Form[Option[String]] =
    knownFactsForm(postcodeMapping(featureFlags.showKfcMtdIt))

  val agentFastTrackDateOfBirthForm: Form[Option[String]] =
    knownFactsForm(dateOfBirthMapping(featureFlags.showKfcPersonalIncome))

  val agentFastTrackVatRegDateForm: Form[Option[String]] =
    knownFactsForm(vatRegDateMapping(featureFlags))

  val showKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      agentSessionCache.fetch.map {
        case Some(agentSession)
            if agentSession.clientIdentifier.nonEmpty && agentSession.clientIdentifierType.nonEmpty && agentSession.service.nonEmpty =>
          Ok(
            known_fact(
              getKnownFactFormForService(agentSession.service.getOrElse("")),
              agentSession.service.getOrElse(""),
              serviceToMessageKey(agentSession.service.getOrElse(""))
            ))

        case Some(_) => throw new Exception("no content in cache")
        case None    => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentSessionCache.get.flatMap(
        agentSession => {
          val service = agentSession.service.getOrElse("")
          getKnownFactFormForService(service)
            .bindFromRequest()
            .fold(
              formWithErrors => Ok(known_fact(formWithErrors, service, serviceToMessageKey(service))),
              kf => {
                val updatedCache = agentSession.copy(knownFact = kf, fromFastTrack = true)
                agentSessionCache
                  .save(updatedCache)
                  .flatMap { _ =>
                    redirectFastTrackToNextPage(arn, updatedCache, isWhitelisted)
                  }
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

  def agentFastTrack: Action[AnyContent] = Action.async { implicit request =>
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
            fastTrackRequest => {
              val agentSession = AgentSession(
                clientType = fastTrackRequest.clientType,
                service = Some(fastTrackRequest.service),
                clientIdentifierType = Some(fastTrackRequest.clientIdentifierType),
                clientIdentifier = Some(fastTrackRequest.clientIdentifier),
                knownFact = fastTrackRequest.knownFact,
                fromFastTrack = true
              )
              agentSessionCache.save(agentSession).flatMap { _ =>
                withMaybeContinueUrlCached {
                  ifShouldShowService(agentSession.service.getOrElse(""), featureFlags, isWhitelisted) {
                    Redirect(routes.AgentsFastTrackInvitationController.showCheckDetails())
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
      agentSessionCache.fetch.map {
        case Some(agentSession) =>
          Ok(
            check_details(
              checkDetailsForm,
              agentSession,
              featureFlags,
              serviceToMessageKey(agentSession.service.getOrElse("")),
              CheckDetailsPageConfig(agentSession, featureFlags)
            ))
        case None => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitCheckDetails: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      val agentSession = agentSessionCache.fetch.map(_.getOrElse(AgentSession()))
      checkDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors => {
            agentSession.flatMap { session =>
              Future successful Ok(
                check_details(
                  formWithErrors,
                  session,
                  featureFlags,
                  serviceToMessageKey(session.service.getOrElse("")),
                  CheckDetailsPageConfig(session, featureFlags)
                ))
            }
          },
          data => {
            if (data.value.getOrElse(false)) {
              agentSession.flatMap(cacheItem => redirectFastTrackToNextPage(arn, cacheItem, isWhitelisted))
            } else Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          }
        )
    }
  }

  private def withMaybeErrorUrlCached[A](
    block: Option[ContinueUrl] => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    continueUrlActions.withMaybeErrorUrl {
      case None => block(None)
      case Some(continueUrl) =>
        agentSessionCache.fetch
          .map(_.getOrElse(AgentSession()))
          .flatMap(session => agentSessionCache.save(session.copy(errorUrl = Some(continueUrl.url))))
          .flatMap(_ => block(Some(continueUrl)))
    }

  private def withMaybeContinueUrlCached[A](
    block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    continueUrlActions.withMaybeContinueUrl {
      case None => block
      case Some(continueUrl) =>
        agentSessionCache.get.flatMap { session =>
          agentSessionCache
            .save(session.copy(continueUrl = Some(continueUrl.url)))
            .flatMap(_ => block)
        }
    }
}

object AgentsFastTrackInvitationController {

  def knownFactsForm(knownFactsMapping: Mapping[Option[String]]) =
    Form(single("knownFact" -> knownFactsMapping))

  val validateFastTrackForm: Constraint[AgentFastTrackRequest] =
    Constraint[AgentFastTrackRequest] { formData: AgentFastTrackRequest =>
      formData match {
        case AgentFastTrackRequest(Some("personal") | None, HMRCMTDIT, "ni", clientId, _) if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(Some("personal") | None, HMRCPIR, "ni", clientId, _) if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(_, HMRCMTDVAT, "vrn", clientId, _) if Vrn.isValid(clientId) => Valid
        case _                                                                                 => Invalid(ValidationError("INVALID_SUBMISSION"))
      }
    }

  val agentFastTrackForm: Form[AgentFastTrackRequest] =
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
        AgentFastTrackRequest(clientTypeFor(clientType, service), service, clientIdType, clientId, knownFact)
      })({ request =>
        Some(
          (
            request.clientType,
            request.service,
            request.clientIdentifierType,
            request.clientIdentifier,
            request.knownFact))
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
