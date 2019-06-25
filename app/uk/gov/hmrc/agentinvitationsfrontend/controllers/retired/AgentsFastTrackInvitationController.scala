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

import javax.inject.{Inject, Singleton}
import play.api.data.Forms.{mapping, optional, single, text}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{InvitationsConnector, RelationshipsConnector, SsoConnector}
import uk.gov.hmrc.agentinvitationsfrontend.controllers._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.retired.AgentsInvitationController.agentConfirmationForm
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.repository.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{CheckDetailsPageConfig, KnownFactPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{check_details, known_fact}
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.binders.{AbsoluteWithHostnameFromWhitelist, RedirectUrl, RedirectUrlPolicy, UnsafePermitAll}
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl._
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrlPolicy.Id

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsFastTrackInvitationController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  relationshipsConnector: RelationshipsConnector,
  ssoConnector: SsoConnector,
  agentSessionCache: AgentSessionCache,
  authActions: AuthActions,
  val redirectUrlActions: RedirectUrlActions,
  auditService: AuditService)(
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
  import AgentsFastTrackInvitationController._
  import authActions._

  val agentFastTrackRoot = routes.AgentsFastTrackInvitationController.agentFastTrack()

  val agentFastTrackPostcodeForm: Form[String] =
    knownFactsForm(postcodeMapping)

  val agentFastTrackDateOfBirthForm: Form[String] =
    knownFactsForm(dateOfBirthMapping)

  val agentFastTrackVatRegDateForm: Form[String] =
    knownFactsForm(vatRegDateMapping)

  val showKnownFact: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.fetch.map {
        case Some(agentSession)
            if agentSession.clientIdentifier.nonEmpty && agentSession.clientIdentifierType.nonEmpty && agentSession.service.nonEmpty =>
          Ok(
            known_fact(
              getKnownFactFormForService(agentSession.service.getOrElse("")),
              KnownFactPageConfig(
                agentSession.service.getOrElse(""),
                serviceToMessageKey(agentSession.service.getOrElse("")),
                getSubmitKnownFactCallBy(agentSession.service.getOrElse("")),
                routes.AgentsFastTrackInvitationController.showCheckDetails().url
              )
            ))

        case Some(_) => throw new Exception("no content in cache")
        case None    => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitKnownFactItsa: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      agentSessionCache.hardGet.flatMap(
        agentSession => {
          val service = agentSession.service.getOrElse("")
          agentFastTrackPostcodeForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Ok(known_fact(
                  formWithErrors,
                  KnownFactPageConfig(
                    service,
                    serviceToMessageKey(service),
                    getSubmitKnownFactCallBy(service),
                    routes.AgentsFastTrackInvitationController.showCheckDetails().url
                  )
                )),
              kf => {
                val updatedCache = agentSession.copy(knownFact = Some(kf), fromFastTrack = true)
                agentSessionCache
                  .save(updatedCache)
                  .flatMap { _ =>
                    redirectFastTrackToNextPage(agent.arn, updatedCache, agent.isWhitelisted)
                  }
              }
            )
        }
      )
    }
  }

  val submitKnownFactIrv: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      agentSessionCache.hardGet.flatMap(
        agentSession => {
          val service = agentSession.service.getOrElse("")
          agentFastTrackDateOfBirthForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Ok(known_fact(
                  formWithErrors,
                  KnownFactPageConfig(
                    service,
                    serviceToMessageKey(service),
                    getSubmitKnownFactCallBy(service),
                    routes.AgentsFastTrackInvitationController.showCheckDetails().url
                  )
                )),
              kf => {
                val updatedCache = agentSession.copy(knownFact = Some(kf), fromFastTrack = true)
                agentSessionCache
                  .save(updatedCache)
                  .flatMap { _ =>
                    redirectFastTrackToNextPage(agent.arn, updatedCache, agent.isWhitelisted)
                  }
              }
            )
        }
      )
    }
  }

  val submitKnownFactVat: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      agentSessionCache.hardGet.flatMap(
        agentSession => {
          val service = agentSession.service.getOrElse("")
          agentFastTrackVatRegDateForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Ok(known_fact(
                  formWithErrors,
                  KnownFactPageConfig(
                    service,
                    serviceToMessageKey(service),
                    getSubmitKnownFactCallBy(service),
                    routes.AgentsFastTrackInvitationController.showCheckDetails().url
                  )
                )),
              kf => {
                val updatedCache = agentSession.copy(knownFact = Some(kf), fromFastTrack = true)
                agentSessionCache
                  .save(updatedCache)
                  .flatMap { _ =>
                    redirectFastTrackToNextPage(agent.arn, updatedCache, agent.isWhitelisted)
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
    withAuthorisedAsAgent { agent =>
      if (featureFlags.enableFastTrack) {
        agentFastTrackForm
          .bindFromRequest()
          .fold(
            formErrors => {
              withMaybeErrorUrlCached {
                case Some(continue) =>
                  Future successful Redirect(
                    continue
                      .get(UnsafePermitAll)
                      .url + s"?issue=${formErrors.errorsAsJson.as[FastTrackErrors].formErrorsMessages}")
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
                fromFastTrack = true,
                clientTypeForInvitationSent = fastTrackRequest.clientType
              )
              agentSessionCache.save(agentSession).flatMap { _ =>
                withMaybeContinueUrlCached {
                  ifShouldShowService(agentSession.service.getOrElse(""), featureFlags, agent.isWhitelisted) {
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
    withAuthorisedAsAgent { _ =>
      agentSessionCache.fetch.map {
        case Some(agentSession) =>
          val fastTrackRequest = AgentFastTrackRequest(
            agentSession.clientType,
            agentSession.service.getOrElse(""),
            agentSession.clientIdentifierType.getOrElse(""),
            agentSession.clientIdentifier.getOrElse(""),
            agentSession.knownFact
          )
          Ok(
            check_details(
              checkDetailsForm,
              CheckDetailsPageConfig(
                fastTrackRequest,
                featureFlags,
                routes.AgentsInvitationController.showClientType(),
                routes.AgentsFastTrackInvitationController.showKnownFact(),
                getSubmitIdentifyClientCallBy(fastTrackRequest.service),
                routes.AgentsFastTrackInvitationController.submitCheckDetails(),
                request.headers.get("Referer")
              )
            ))
        case None => Redirect(routes.AgentsInvitationController.showClientType())
      }
    }
  }

  val submitCheckDetails: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { agent =>
      val agentSession = agentSessionCache.fetch.map(_.getOrElse(AgentSession()))
      checkDetailsForm
        .bindFromRequest()
        .fold(
          formWithErrors => {
            agentSession.flatMap {
              session =>
                val fastTrackRequest = AgentFastTrackRequest(
                  session.clientType,
                  session.service.getOrElse(""),
                  session.clientIdentifierType.getOrElse(""),
                  session.clientIdentifier.getOrElse(""),
                  session.knownFact
                )
                Future successful Ok(check_details(
                  formWithErrors,
                  CheckDetailsPageConfig(
                    fastTrackRequest,
                    featureFlags,
                    routes.AgentsInvitationController.showClientType(),
                    routes.AgentsFastTrackInvitationController.showKnownFact(),
                    getSubmitIdentifyClientCallBy(fastTrackRequest.service),
                    routes.AgentsFastTrackInvitationController.submitCheckDetails(),
                    None
                  )
                ))
            }
          },
          data => {
            if (data.choice) {
              agentSession.flatMap(cacheItem => redirectFastTrackToNextPage(agent.arn, cacheItem, agent.isWhitelisted))
            } else Future successful Redirect(routes.AgentsInvitationController.showIdentifyClient())
          }
        )
    }
  }

  private def withMaybeErrorUrlCached[A](
    block: Option[RedirectUrl] => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    redirectUrlActions.maybeRedirectUrlOrBadRequest(redirectUrlActions.getErrorUrl) {
      case None => block(None)
      case Some(redirectUrl) =>
        agentSessionCache.fetch
          .map(_.getOrElse(AgentSession()))
          .flatMap(session => agentSessionCache.save(session.copy(errorUrl = Some(redirectUrl))))
          .flatMap(_ => block(Some(RedirectUrl(redirectUrl))))
    }

  private def withMaybeContinueUrlCached[A](
    block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    redirectUrlActions.maybeRedirectUrlOrBadRequest(redirectUrlActions.getRedirectUrl) {
      case None => block
      case Some(redirectUrl) =>
        agentSessionCache.hardGet.flatMap { session =>
          agentSessionCache
            .save(session.copy(continueUrl = Some(redirectUrl)))
            .flatMap(_ => block)
        }
    }
}

object AgentsFastTrackInvitationController {

  def knownFactsForm(knownFactsMapping: Mapping[String]) =
    Form(single("knownFact" -> knownFactsMapping))

  val validateFastTrackForm: Constraint[AgentFastTrackRequest] =
    Constraint[AgentFastTrackRequest] { formData: AgentFastTrackRequest =>
      formData match {
        case AgentFastTrackRequest(Some(ClientType.personal) | None, HMRCMTDIT, "ni", clientId, _)
            if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(Some(ClientType.personal) | None, HMRCPIR, "ni", clientId, _)
            if Nino.isValid(clientId) =>
          Valid
        case AgentFastTrackRequest(_, HMRCMTDVAT, "vrn", clientId, _) if Vrn.isValid(clientId) => Valid
        case _                                                                                 => Invalid(ValidationError("INVALID_SUBMISSION"))
      }
    }

  val agentFastTrackForm: Form[AgentFastTrackRequest] =
    Form(
      mapping(
        "clientType" -> optional(
          lowerCaseText
            .verifying("UNSUPPORTED_CLIENT_TYPE", Set("personal", "business").contains _)
            .transform(ClientType.toEnum, ClientType.fromEnum)),
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

  def clientTypeFor(clientType: Option[ClientType], service: String): Option[ClientType] =
    clientType.orElse(service match {
      case "HMRC-MTD-IT"            => Some(ClientType.personal)
      case "PERSONAL-INCOME-RECORD" => Some(ClientType.personal)
      case _                        => None
    })

  val checkDetailsForm: Form[Confirmation] = agentConfirmationForm("error.confirmDetails.invalid")

  def getSubmitKnownFactCallBy(service: String) =
    service match {
      case HMRCMTDIT  => routes.AgentsFastTrackInvitationController.submitKnownFactItsa()
      case HMRCPIR    => routes.AgentsFastTrackInvitationController.submitKnownFactIrv()
      case HMRCMTDVAT => routes.AgentsFastTrackInvitationController.submitKnownFactVat()
    }

  def getSubmitIdentifyClientCallBy(service: String) =
    service match {
      case HMRCMTDIT  => routes.AgentsInvitationController.submitIdentifyClientItsa()
      case HMRCPIR    => routes.AgentsInvitationController.submitIdentifyClientIrv()
      case HMRCMTDVAT => routes.AgentsInvitationController.submitIdentifyClientVat()
    }
}
