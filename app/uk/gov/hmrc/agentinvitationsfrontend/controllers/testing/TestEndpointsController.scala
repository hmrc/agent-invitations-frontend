/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, optional, text}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.AgentsInvitationController.normalizedText
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, PasscodeVerification, routes => agentRoutes}
import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentInvitationInput
import uk.gov.hmrc.agentinvitationsfrontend.services.FastTrackCache
import uk.gov.hmrc.agentinvitationsfrontend.views.html.testing.{create_relationship, delete_relationship, test_fast_track}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

class TestEndpointsController @Inject()(
  val messagesApi: play.api.i18n.MessagesApi,
  pirRelationshipConnector: PirRelationshipConnector,
  fastTrackCache: FastTrackCache,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls)
    extends FrontendController with I18nSupport with AuthActions {

  import TestEndpointsController._

  def getDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(delete_relationship(testRelationshipForm))
    }
  }

  def submitDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      testRelationshipForm
        .bindFromRequest()
        .fold(
          formWithErrors => Future successful BadRequest(delete_relationship(formWithErrors)),
          validFormData => {
            pirRelationshipConnector
              .deleteRelationship(validFormData.arn, validFormData.service, validFormData.clientId)
              .map {
                case OK => Redirect(routes.TestEndpointsController.getDeleteRelationship())
                case _  => Redirect(agentRoutes.AgentsInvitationController.notMatched())
              }
          }
        )
    }
  }

  def getCreateRelationship: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(create_relationship(testRelationshipForm))
    }
  }

  def submitCreateRelationship: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      testRelationshipForm
        .bindFromRequest()
        .fold(
          formWithErrors ⇒ Future successful BadRequest(create_relationship(formWithErrors)),
          validFormData => {
            pirRelationshipConnector
              .createRelationship(validFormData.arn, validFormData.service, validFormData.clientId)
              .map {
                case CREATED => Redirect(routes.TestEndpointsController.getCreateRelationship())
                case _       => Redirect(agentRoutes.AgentsInvitationController.notMatched())
              }
          }
        )
    }
  }

  def getFastTrackForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Ok(test_fast_track(testAgentFastTrackForm))
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

  val testAgentFastTrackForm: Form[CurrentInvitationInput] = {
    Form(
      mapping(
        "service"              -> optional(text),
        "clientIdentifierType" -> optional(text),
        "clientIdentifier"     -> optional(normalizedText),
        "postcode"             -> optional(text),
        "vatRegDate"           -> optional(text)
      )({ (service, clientIdType, clientId, postcode, vatRegDate) =>
        CurrentInvitationInput(service, clientIdType, clientId, postcode, vatRegDate)
      })({ fastTrack =>
        Some(
          (
            fastTrack.service,
            fastTrack.clientIdentifierType,
            fastTrack.clientIdentifier,
            fastTrack.postcode,
            fastTrack.vatRegDate))
      }))
  }
}
