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
import play.api.data.Forms.{mapping, text}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.views.html.testing.delete_relationship
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{routes => agentRoutes}

import scala.concurrent.Future

class TestEndpointsController @Inject()(val messagesApi: play.api.i18n.MessagesApi,
                                        rsConnector: PirRelationshipConnector
                                       )(implicit val configuration: Configuration, externalUrls: ExternalUrls)
  extends FrontendController with I18nSupport {

  import TestEndpointsController._

  def getDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(delete_relationship(testDeleteRelationshipForm))
  }

  def submitDeleteRelationship: Action[AnyContent] = Action.async { implicit request =>
    testDeleteRelationshipForm.bindFromRequest().fold(
      formWithErrors => Future successful BadRequest(delete_relationship(formWithErrors)),
      validFormData => {
        rsConnector.deleteRelationship(validFormData.arn, validFormData.service, validFormData.clientId).map {
          case OK => Redirect(routes.TestEndpointsController.getDeleteRelationship())
          case _ => Redirect(agentRoutes.AgentsInvitationController.notMatched())
        }
      }
    )
  }

  def getCreateRelationship: Action[AnyContent] = ???

  //    Action.async { implicit request =>
  //    Future successful Ok(create_relationship(createRelationship))
  //  }

  def submitCreateRelationship: Action[AnyContent] = ???

  //    Action.async { implicit request =>
  //    createRelationship.bindFromRequest().fold(
  //      formWithErrors ⇒ failedForm(formWithErrors),
  //      validFormData => {
  //        rsConnector.createRelationship(validFormData.arn, validFormData.service, validFormData.clientId, LocalDateTime.now.toString).map(response ⇒
  //          response.fold(_ ⇒ Redirect(agentRoutes.AgentController.getErrorPage()),
  //            _ ⇒ Redirect(routes.TestEndpointsController.submitCreateRelationship())
  //          ))
  //      }
  //    )
  //  }

  def getCreateRelationshipSuccess: Action[AnyContent] = ???

  //    Action.async { implicit request =>
  //    Future successful Ok(select_client_success("We have created a relationship"))
  //  }
}

object TestEndpointsController {

  case class CreateRelationship(arn: Arn, service: String, clientId: String)

  case class DeleteRelationship(arn: Arn, service: String, clientId: String)

  val testDeleteRelationshipForm: Form[DeleteRelationship] = {
    Form(mapping(
      "arn" -> text,
      "service" -> text,
      "clientId" -> text
    )({
      case (arn, service, clientId) => DeleteRelationship(Arn(arn), service, clientId)
    })({
      dr: DeleteRelationship => Some((dr.arn.value, dr.service, dr.clientId))
    }))
  }
}