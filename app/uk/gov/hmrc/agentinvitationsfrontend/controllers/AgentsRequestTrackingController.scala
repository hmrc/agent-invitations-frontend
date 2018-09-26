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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.{Inject, Named, Singleton}
import org.joda.time.LocalDate
import play.api.{Configuration, Logger}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.services.TrackService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.recent_invitations
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

@Singleton
class AgentsRequestTrackingController @Inject()(
  val auditService: AuditService,
  val messagesApi: play.api.i18n.MessagesApi,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification,
  val featureFlags: FeatureFlags,
  val trackService: TrackService,
  @Named("track-requests-show-last-days") val trackRequestsShowLastDays: Int)(
  implicit val externalUrls: ExternalUrls,
  configuration: Configuration)
    extends FrontendController with I18nSupport with AuthActions {

  val showTrackRequests: Action[AnyContent] = Action.async { implicit request =>
    if (featureFlags.enableTrackRequests) {
      withAuthorisedAsAgent { (arn, isWhitelisted) =>
        implicit val now: LocalDate = LocalDate.now()
        for {
          invitationsAndRelationships <- trackService.bindInvitationsAndRelationships(
                                          arn,
                                          isWhitelisted,
                                          trackRequestsShowLastDays)
        } yield Ok(recent_invitations(invitationsAndRelationships, trackRequestsShowLastDays))
      }
    } else {
      Logger(getClass).warn("Feature flag to enable track page is off")
      Future successful BadRequest
    }
  }
}
