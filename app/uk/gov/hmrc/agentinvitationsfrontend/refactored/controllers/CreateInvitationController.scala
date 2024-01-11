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

package uk.gov.hmrc.agentinvitationsfrontend.refactored.controllers

import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.refactored.services.CustomerDataCheckService
import uk.gov.hmrc.agentservice.AgentServiceSupport
import uk.gov.hmrc.agentservice.CustomerDataCheckResponse.writes
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.agentinvitationsfrontend.refactored.views.html._
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig

import javax.inject.{Inject, Provider, Singleton}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

@Singleton
class CreateInvitationController @Inject()(
  cc: MessagesControllerComponents,
  agentServiceSupport: AgentServiceSupport,
  customerDataCheckService: CustomerDataCheckService,
  identifyClientView: identify_client,
  ecp: Provider[ExecutionContextExecutor])(implicit externalUrls: ExternalUrls, contactFrontendConfig: ContactFrontendConfig)
    extends FrontendController(cc) {

  implicit val ec: ExecutionContext = ecp.get
  def showIdentifyClient(service: String): Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok(identifyClientView(agentServiceSupport.identifyClientPageConfig(service)))
  }

  def submitIdentifyClient(service: String): Action[AnyContent] = Action.async { implicit request =>
    {

      agentServiceSupport
        .identifyClientPageConfig(service)
        .form
        .bindFromRequest
        .fold(
          errors => {
            Future successful Ok(identifyClientView(agentServiceSupport.identifyClientPageConfig(service, Some(errors))))
          },
          data =>
            customerDataCheckService
              .customerDataCheck(service, data.clientId, data.knownFact)
              .map(res => Ok(Json.toJson(res)))
        )
    }
  }
}
