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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.personalincomerecord

import javax.inject.{Inject, Named}

import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, PasscodeVerification}
import uk.gov.hmrc.agentinvitationsfrontend.models.RadioConfirm
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients.pirRelationships._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.error_template
import uk.gov.hmrc.auth.core.{AuthConnector, InsufficientConfidenceLevel, InsufficientEnrolments}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

class PirClientRelationshipController @Inject()(
                                                 @Named("agent-invitations-frontend.base-url") externalUrl: String,
                                                 auditService: AuditService,
                                                 afiRelationshipConnector: PirRelationshipConnector,
                                                 val messagesApi: play.api.i18n.MessagesApi,
                                                 val authConnector: AuthConnector,
                                                 val withVerifiedPasscode: PasscodeVerification)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  def deauthoriseAllStart(): Action[AnyContent] = Action.async {
    implicit request =>
      authorisedAsClient { clientId =>
        afiRelationshipConnector.getClientRelationships("PERSONAL-INCOME-RECORD", clientId).map {
          case Some(_) => Ok(client_ends_relationship(RadioConfirm.confirmDeauthoriseRadioForm))
          case None => Redirect(routes.PirClientRelationshipController.getClientEndsRelationshipNoAgentPage)
        }
      }
  }

  def submitDeauthoriseAll(): Action[AnyContent] = Action.async { implicit request =>
    authorisedAsClient { clientId =>
      RadioConfirm.confirmDeauthoriseRadioForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(client_ends_relationship(formWithErrors))
        }, data => {
          if (data.value.getOrElse(false))
            afiRelationshipConnector.terminateAllClientIdRelationships("PERSONAL-INCOME-RECORD", clientId).map {
              case 200 => Ok(client_ends_relationship_ended())
              case 404 => Logger.warn(s"Connector failed to terminate relationships for service: PIR, nino: $clientId")
                Ok(error_template(Messages("error.terminate.404.title"),
                  Messages("error.terminate.404.heading"), Messages("error.terminate.404.message")))
            }
          else Future.successful(Redirect(routes.PirClientRelationshipController.getClientDeclinedRelationshipTermination))
        }
      )
    }
  }

  def getClientDeclinedRelationshipTermination: Action[AnyContent] = Action.async {
    implicit request =>
      Future.successful(Ok(client_cancelled_deauth()))
  }

  def getClientEndsRelationshipNoAgentPage: Action[AnyContent] = Action.async {
    implicit request =>
      Future.successful(Ok(client_ends_relationship_no_agent()))
  }

  private def authorisedAsClient[A](body: String => Future[Result])(implicit request: Request[A], hc: HeaderCarrier) =
    withAuthorisedAsClient("HMRC-NI", "NINO")(body)
}