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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.testing

import javax.inject.Inject
import play.api.{Configuration, Environment, Mode}
import play.api.data.Form
import play.api.data.Forms.{mapping, optional, text}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.{normalizedText, validateClientId}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, CancelAuthorisationForm, CancelRequestForm, DateFieldHelper, PasscodeVerification, TrackResendForm, routes => agentRoutes}
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{supportedClientTypes, supportedServices}
import uk.gov.hmrc.agentinvitationsfrontend.services.CurrentAuthorisationRequestCache
import uk.gov.hmrc.agentinvitationsfrontend.views.html.testing.{create_relationship, delete_relationship, test_fast_track}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future
import scala.util.control.NonFatal

class TestEndpointsController @Inject()(
  val messagesApi: play.api.i18n.MessagesApi,
  pirRelationshipConnector: PirRelationshipConnector,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  val authConnector: AuthConnector,
  val env: Environment,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls)
    extends FrontendController with I18nSupport with AuthActions {

  import TestEndpointsController._

  val isDevEnv: Boolean =
    if (env.mode.equals(Mode.Test)) false else configuration.getString("run.mode").forall(Mode.Dev.toString.equals)

  def getDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(delete_relationship(testRelationshipForm))
  }

  def submitDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    testRelationshipForm
      .bindFromRequest()
      .fold(
        formWithErrors => Future successful BadRequest(delete_relationship(formWithErrors)),
        validFormData => {
          pirRelationshipConnector
            .testOnlyDeleteRelationship(validFormData.arn, validFormData.service, validFormData.clientId)
            .map {
              case Some(true) => Redirect(routes.TestEndpointsController.getDeleteRelationship())
              case _          => Redirect(agentRoutes.AgentsErrorController.notMatched())
            }
        }
      )
  }

  def getCreateRelationship: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(create_relationship(testRelationshipForm))
  }

  def submitCreateRelationship: Action[AnyContent] = Action.async { implicit request =>
    testRelationshipForm
      .bindFromRequest()
      .fold(
        formWithErrors ⇒ Future successful BadRequest(create_relationship(formWithErrors)),
        validFormData => {
          pirRelationshipConnector
            .testOnlyCreateRelationship(validFormData.arn, validFormData.service, validFormData.clientId)
            .map {
              case CREATED => Redirect(routes.TestEndpointsController.getCreateRelationship())
              case _       => Redirect(agentRoutes.AgentsErrorController.notMatched())
            }
        }
      )
  }

  def getFastTrackForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(test_fast_track(testCurrentAuthorisationRequestForm, isDevEnv))
    }
  }
}

object TestEndpointsController {

  case class Relationship(arn: Arn, service: String, clientId: String)

  val testRelationshipForm: Form[Relationship] = {
    Form(
      mapping(
        "arn"      -> text,
        "service"  -> text,
        "clientId" -> text
      )({
        case (arn, service, clientId) => Relationship(Arn(arn), service, clientId)
      })({ dr: Relationship =>
        Some((dr.arn.value, dr.service, dr.clientId))
      }))
  }

  val testCurrentAuthorisationRequestForm: Form[CurrentAuthorisationRequest] = {
    Form(
      mapping(
        "clientType"           -> optional(text),
        "service"              -> text,
        "clientIdentifierType" -> text,
        "clientIdentifier"     -> normalizedText,
        "knownFact"            -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientType, service, clientIdType, clientId, knownFact)
      })({ fastTrack =>
        Some(
          (
            fastTrack.clientType,
            fastTrack.service,
            fastTrack.clientIdentifierType,
            fastTrack.clientIdentifier,
            fastTrack.knownFact))
      }))
  }

  val testTrackInformationForm: Form[TrackResendForm] = {
    Form(
      mapping(
        "service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientType" -> optional(text)
          .verifying("Unsupported client type", clientType => supportedClientTypes.contains(clientType)),
        "expiryDate" -> text.verifying("Invalid date format", expiryDate => DateFieldHelper.parseDate(expiryDate))
      )(TrackResendForm.apply)(TrackResendForm.unapply))
  }

  val testCancelRequestForm: Form[CancelRequestForm] = {
    Form(
      mapping(
        "invitationId" -> text.verifying("Invalid invitation Id", invitationId => InvitationId.isValid(invitationId)),
        "service"      -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientName"   -> text
      )(CancelRequestForm.apply)(CancelRequestForm.unapply)
    )
  }

  val testCancelAuthorisationForm: Form[CancelAuthorisationForm] = {
    Form(
      mapping(
        "service"    -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "clientId"   -> normalizedText.verifying(validateClientId),
        "clientName" -> text
      )(CancelAuthorisationForm.apply)(CancelAuthorisationForm.unapply))
  }
}
