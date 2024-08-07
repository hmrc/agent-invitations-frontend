/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.data.Form
import play.api.data.Forms.{boolean, mapping, optional, text}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.api.{Configuration, Environment, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, CancelAuthorisationForm, CancelRequestForm, DateFieldHelper, TrackResendForm, routes => agentRoutes}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentFastTrackRequest, ClientType}
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.testing.{create_relationship, delete_relationship, test_fast_track}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, ClientIdType, ClientIdentifier, InvitationId, Service}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TestEndpointsController @Inject() (
  val authActions: AuthActions,
  pirRelationshipConnector: PirRelationshipConnector,
  val authConnector: AuthConnector,
  val env: Environment,
  deleteRelationshipView: delete_relationship,
  createRelationshipView: create_relationship,
  testFastTrackView: test_fast_track
)(implicit
  val config: Configuration,
  implicit val contactFrontendConfig: ContactFrontendConfig,
  val externalUrls: ExternalUrls,
  ec: ExecutionContext,
  val cc: MessagesControllerComponents,
  val appConfig: AppConfig
) extends FrontendController(cc) with I18nSupport {

  import TestEndpointsController._

  val isLocalEnv =
    if (env.mode.equals(Mode.Test)) false else env.mode.equals(Mode.Dev)

  def getDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(deleteRelationshipView(testRelationshipForm))
  }

  def submitDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    testRelationshipForm
      .bindFromRequest()
      .fold(
        formWithErrors => Future successful BadRequest(deleteRelationshipView(formWithErrors)),
        validFormData =>
          pirRelationshipConnector
            .testOnlyDeleteRelationship(validFormData.arn, validFormData.service, validFormData.clientId)
            .map {
              case Some(true) => Redirect(routes.TestEndpointsController.getDeleteRelationship())
              case _          => Redirect(agentRoutes.AgentInvitationJourneyController.showNotMatched)
            }
      )
  }

  def getCreateRelationship: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(createRelationshipView(testRelationshipForm))
  }

  def submitCreateRelationship: Action[AnyContent] = Action.async { implicit request =>
    testRelationshipForm
      .bindFromRequest()
      .fold(
        formWithErrors ⇒ Future successful BadRequest(createRelationshipView(formWithErrors)),
        validFormData =>
          pirRelationshipConnector
            .testOnlyCreateRelationship(validFormData.arn, validFormData.service, validFormData.clientId)
            .map {
              case CREATED => Redirect(routes.TestEndpointsController.getCreateRelationship)
              case _       => Redirect(agentRoutes.AgentInvitationJourneyController.showNotMatched)
            }
      )
  }

  def getFastTrackForm: Action[AnyContent] = Action.async { implicit request =>
    authActions.withAuthorisedAsAgent { _ =>
      Future successful Ok(testFastTrackView(testCurrentAuthorisationRequestForm, isLocalEnv))
    }
  }
}

object TestEndpointsController {

  case class Relationship(arn: Arn, service: String, clientId: String)

  val testRelationshipForm: Form[Relationship] =
    Form(
      mapping(
        "arn"      -> text,
        "service"  -> text,
        "clientId" -> text
      ) { case (arn, service, clientId) =>
        Relationship(Arn(arn), service, clientId)
      } { dr: Relationship =>
        Some((dr.arn.value, dr.service, dr.clientId))
      }
    )

  val testCurrentAuthorisationRequestForm: Form[AgentFastTrackRequest] =
    Form(
      mapping(
        "clientType"           -> optional(text.transform(ClientType.toEnum, ClientType.fromEnum)),
        "service"              -> text.transform[Service](Service.forId, _.id),
        "clientIdentifierType" -> text,
        "clientIdentifier"     -> normalizedText,
        "knownFact"            -> optional(text)
      ) { (clientType, service, clientIdType, clientId, knownFact) =>
        AgentFastTrackRequest(clientType, service, ClientIdType.forId(clientIdType).createUnderlying(clientId), knownFact)
      } { fastTrack =>
        Some((fastTrack.clientType, fastTrack.service, ClientIdentifier(fastTrack.clientId).typeId, fastTrack.clientId.value, fastTrack.knownFact))
      }
    )

  val testTrackInformationForm: Form[TrackResendForm] =
    Form(
      mapping(
        "service" -> text.verifying("Unsupported Service", service => supportedServices.exists(_.id == service)),
        "clientType" -> optional(
          text
            .verifying("Unsupported client type", ClientType.isValid(_))
            .transform(ClientType.toEnum, ClientType.fromEnum)
        ),
        "expiryDate" -> text.verifying("Invalid date format", expiryDate => DateFieldHelper.parseDate(expiryDate)),
        "isAltItsa"  -> boolean
      )(TrackResendForm.apply)(TrackResendForm.unapply)
    )

  val testCancelRequestForm: Form[CancelRequestForm] =
    Form(
      mapping(
        "invitationId" -> text.verifying("Invalid invitation Id", invitationId => InvitationId.isValid(invitationId)),
        "service"      -> text.verifying("Unsupported Service", service => supportedServices.exists(_.id == service)),
        "clientType" -> text
          .verifying("Unsupported client type", ClientType.isValid(_)),
        "clientName" -> text
      )(CancelRequestForm.apply)(CancelRequestForm.unapply)
    )

  val testCancelAuthorisationForm: Form[CancelAuthorisationForm] =
    Form(
      mapping(
        "service"  -> text.verifying("Unsupported Service", service => supportedServices.exists(_.id == service)),
        "clientId" -> normalizedText.verifying(validateClientId),
        "clientType" -> text
          .verifying("Unsupported ClientType", ClientType.isValid(_)),
        "clientName"   -> text,
        "invitationId" -> text,
        "status"       -> text.verifying("Unexpected InvitationStatus", status => status == "Accepted" || status == "Partialauth")
      )(CancelAuthorisationForm.apply)(CancelAuthorisationForm.unapply)
    )
}
