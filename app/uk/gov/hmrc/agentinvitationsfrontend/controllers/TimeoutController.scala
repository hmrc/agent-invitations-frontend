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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.timed_out
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class TimeoutController @Inject()(timedOutTemplate: timed_out, authActions: AuthActionsImpl)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  val mcc: MessagesControllerComponents,
  ec: ExecutionContext,
  val appConfig: AppConfig)
    extends FrontendController(mcc) {

  def keepAlive: Action[AnyContent] = Action.async { implicit request =>
    Future successful Ok("OK")
  }
}
