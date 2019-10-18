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

import java.net.URL

import javax.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, optional, text}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, CancelAuthorisationForm, CancelRequestForm, DateFieldHelper, PasscodeVerification, TrackResendForm, routes => agentRoutes}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{supportedClientTypes, supportedServices}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, ClientType}
import uk.gov.hmrc.agentinvitationsfrontend.repository.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.testing.{create_relationship, delete_relationship, test_fast_track}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class TestEndpointsController @Inject()(
  val messagesApi: play.api.i18n.MessagesApi,
  pirRelationshipConnector: PirRelationshipConnector,
  agentSessionCache: AgentSessionCache,
  val authConnector: AuthConnector,
  val env: Environment,
  val withVerifiedPasscode: PasscodeVerification,
  val pdvBaseUrl: URL)(implicit val config: Configuration, val externalUrls: ExternalUrls, ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  import TestEndpointsController._

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
              case _          => Redirect(agentRoutes.AgentInvitationJourneyController.showNotMatched())
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
              case _       => Redirect(agentRoutes.AgentInvitationJourneyController.showNotMatched())
            }
        }
      )
  }

  def getFastTrackForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
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

  val testCurrentAuthorisationRequestForm: Form[AgentFastTrackRequest] = {
    Form(
      mapping(
        "clientType"           -> optional(text.transform(ClientType.toEnum, ClientType.fromEnum)),
        "service"              -> text,
        "clientIdentifierType" -> text,
        "clientIdentifier"     -> normalizedText,
        "knownFact"            -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        AgentFastTrackRequest(clientType, service, clientIdType, clientId, knownFact)
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
        "clientType" -> optional(
          text
            .verifying("Unsupported client type", clientType => supportedClientTypes.contains(clientType))
            .transform(ClientType.toEnum, ClientType.fromEnum)),
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
