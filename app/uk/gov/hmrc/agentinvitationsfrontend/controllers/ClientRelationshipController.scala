/*
 * Copyright 2017 HM Revenue & Customs
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

import javax.inject.{Inject, Named}

import play.api.{Configuration, Logger}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors.AfiRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.RadioConfirm
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients.afiRelationships._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.http.NotFoundException

import scala.concurrent.Future

class ClientRelationshipController @Inject()(
                                              @Named("agent-invitations-frontend.base-url") externalUrl: String,
                                              auditService: AuditService,
                                              afiRelationshipConnector: AfiRelationshipConnector,
                                              val messagesApi: play.api.i18n.MessagesApi)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport {

  def afiDeauthoriseAllStart(): Action[AnyContent] = Action.async {
    implicit request =>
      //implicit nino =>
      afiRelationshipConnector.getAfiClientRelationships("afi", "AA123456Z").map { hasRelationships =>
        Ok(client_ends_relationship(RadioConfirm.confirmInvitationForm))
      }.recover {
        case ex: NotFoundException => Redirect(routes.ClientRelationshipController.getClientEndsRelationshipNoAgentPage)
      }
  }

  def getClientEndsRelationshipNoAgentPage: Action[AnyContent] = Action.async {
    implicit request =>
      Future.successful(Ok(client_ends_relationship_no_agent()))
  }

  def getErrorMessage: Action[AnyContent] = Action.async {
    implicit request =>
      Future.successful(Ok(failure_message()))
  }


  def testTerminate(): Action[AnyContent] = Action.async {
    implicit request =>
      //     Future.successful (Ok("aa"))
      afiRelationshipConnector.afiTerminateAllClientIdRelationships("afi", "AA123456A").map {
        case Ok => Ok("aa")
        case _ => Ok("be")
      }
  }


//  def submitAfiDeauthoriseAll(): Action[AnyContent] = authoriseForAgentsForIndividuals { implicit request =>
//    //implicit nino =>
//    RadioConfirm.confirmInvitationForm.bindFromRequest().fold(
//      formWithErrors => {
//        Future successful Ok(client_ends_relationship(formWithErrors))
//      }, data => {
//        if (data.value.getOrElse(false)) //Ok("aa")
//          afiRelationshipConnector.afiTerminateAllClientIdRelationships("afi", nino).map {
//            case Ok => Ok(client_ends_relationship_ended())
//            case NotFound => Logger.warn(s"Connector failed to terminate relationships for service: Afi, nino: SOMENINOHERE")//$nino.")
//              Redirect(routes.ClientRelationshipController.getErrorMessage())
//          }
//        else Future successful Ok(client_cancelled_deauth())
//      }
//    )
//  }
}
