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
import javax.inject.Inject
import play.api.Configuration
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

class ClientErrorController @Inject()(override val messagesApi: play.api.i18n.MessagesApi)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls)
    extends FrontendController with I18nSupport {

  val incorrectClientType: Action[AnyContent] = Action.async { implicit request =>
    request.session.get("clientType") match {
      case Some(clientType) => Future successful Forbidden(incorrect_client_type(clientType))
      case _                => Future successful Redirect(routes.ClientErrorController.notAuthorised())
    }
  }

//  val notSignedUp: Action[AnyContent] = Action { implicit request =>
//    request.session.get("clientService") match {
//      case Some(Services.HMRCMTDVAT) =>
//        Forbidden(not_signed_up(Messages("not-signed-up-vat.description"), Services.messageKeyForVAT))
//      case Some(Services.HMRCMTDIT) =>
//        Forbidden(not_signed_up(Messages("not-signed-up.description"), Services.messageKeyForITSA))
//      case _ =>
//        Forbidden(not_signed_up(Messages("not-signed-up.description"), "Service is Missing"))
//    }
//  }

  val notAuthorised: Action[AnyContent] = Action { implicit request =>
    Forbidden(
      not_authorised(
        Messages("not-authorised.header"),
        Messages("not-authorised.description"),
        Services.messageKeyForAfi))
  }

//  val incorrectInvitation: Action[AnyContent] = Action { implicit request =>
//    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
//    Forbidden(incorrect_invitation(serviceMessageKey))
//  }

  val notFoundInvitation: Action[AnyContent] = Action { implicit request =>
    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
    NotFound(not_found_invitation(serviceMessageKey))
  }

//  val invitationAlreadyResponded: Action[AnyContent] = Action { implicit request =>
//    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
//    Forbidden(invitation_already_responded(serviceMessageKey))
//  }

//  val invitationExpired: Action[AnyContent] = Action { implicit request =>
//    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
//    Ok(invitation_expired(serviceMessageKey))
//  }
//
//  val requestCancelled: Action[AnyContent] = Action { implicit request =>
//    val serviceMessageKey = request.session.get("clientService").getOrElse("Service Is Missing")
//    Ok(request_cancelled(serviceMessageKey))
//  }

}
