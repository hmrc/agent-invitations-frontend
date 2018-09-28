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
import play.api.data.Form
import play.api.data.Forms.{mapping, text}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.supportedServices
import uk.gov.hmrc.agentinvitationsfrontend.services.TrackService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.track.{recent_invitations, resend_link}
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

case class TrackResendForm(service: String, invitationId: String, expiryDate: String)

@Singleton
class AgentsRequestTrackingController @Inject()(
  val auditService: AuditService,
  val messagesApi: play.api.i18n.MessagesApi,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification,
  val featureFlags: FeatureFlags,
  val trackService: TrackService,
  @Named("track-requests-show-last-days") val trackRequestsShowLastDays: Int,
  @Named("agent-invitations-frontend.external-url") externalUrl: String)(
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

  def postToResendLink: Action[AnyContent] = Action.async { implicit request =>
    trackInformationForm
      .bindFromRequest()
      .fold(
        _ => {
          Logger(getClass).error("Error in form when redirecting to resend-link page.")
          Future successful BadRequest
        },
        data => Future successful Ok(resend_link(data.service, data.invitationId, data.expiryDate, externalUrl))
      )
  }

  val trackInformationForm: Form[TrackResendForm] = {
    Form(
      mapping(
        "service"      -> text.verifying("Unsupported Service", service => supportedServices.contains(service)),
        "invitationId" -> text.verifying("Invalid invitation Id", invitationId => InvitationId.isValid(invitationId)),
        "expiryDate"   -> text.verifying("Invalid date format", expiryDate => DateFieldHelper.parseDate(expiryDate))
      )(TrackResendForm.apply)(TrackResendForm.unapply))
  }

}
