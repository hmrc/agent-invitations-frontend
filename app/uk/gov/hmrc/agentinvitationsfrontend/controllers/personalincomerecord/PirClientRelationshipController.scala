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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.personalincomerecord

import javax.inject.{Inject, Named, Singleton}

import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, PasscodeVerification}
import uk.gov.hmrc.agentinvitationsfrontend.models.RadioConfirm
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients.pirRelationships._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.error_template
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

@Singleton
class PirClientRelationshipController @Inject()(
                                                 @Named("agent-invitations-frontend.external-url") externalUrl: String,
                                                 auditService: AuditService,
                                                 afiRelationshipConnector: PirRelationshipConnector,
                                                 val messagesApi: play.api.i18n.MessagesApi,
                                                 val authConnector: AuthConnector,
                                                 val withVerifiedPasscode: PasscodeVerification)
                                               (implicit val configuration: Configuration, externalUrls: ExternalUrls)
  extends FrontendController with I18nSupport with AuthActions {

  val deauthoriseAllStart: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthorisedAsClient("HMRC-NI", "NINO") { clientId =>
        afiRelationshipConnector.getClientRelationships("PERSONAL-INCOME-RECORD", clientId).map {
          case Some(_) => Ok(client_ends_relationship(RadioConfirm.confirmDeauthoriseRadioForm))
          case None => Redirect(routes.PirClientRelationshipController.getClientEndsRelationshipNoAgentPage())
        }
      }
  }

  val submitDeauthoriseAll: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsClient("HMRC-NI", "NINO") { clientId =>
      RadioConfirm.confirmDeauthoriseRadioForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(client_ends_relationship(formWithErrors))
        }, data => {
          if (data.value.getOrElse(false))
            afiRelationshipConnector.terminateAllClientIdRelationships("PERSONAL-INCOME-RECORD", clientId).map {
              case 200 => Ok(client_ends_relationship_ended())
              case 500 => Logger.warn(s"Connector failed to terminate relationships for service: PIR, nino: $clientId")
                Ok(error_template(Messages("error.terminate.500.title"),
                  Messages("error.terminate.500.heading"), Messages("error.terminate.500.message")))
            }
          else Future.successful(Redirect(routes.PirClientRelationshipController.getClientDeclinedRelationshipTermination()))
        }
      )
    }
  }

  val getClientDeclinedRelationshipTermination: Action[AnyContent] = Action.async {
    implicit request =>
      Future.successful(Ok(client_cancelled_deauth()))
  }

  val getClientEndsRelationshipNoAgentPage: Action[AnyContent] = Action.async {
    implicit request =>
      Future.successful(Ok(client_ends_relationship_no_agent()))
  }
}