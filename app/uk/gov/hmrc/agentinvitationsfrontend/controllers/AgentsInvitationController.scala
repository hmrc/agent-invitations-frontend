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

import org.joda.time.format.DateTimeFormat
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation._
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.domain.Nino.isValid
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services.{HMRCMTDIT, HMRCPIR}

import scala.concurrent.Future

@Singleton
class AgentsInvitationController @Inject()(
                                            @Named("agent-invitations-frontend.external-url") externalUrl: String,
                                            @Named("agent-services-account-frontend.external-url") asAccUrl: String,
                                            @Named("features.show-hmrc-mtd-it") showHmrcMtdIt: Boolean,
                                            @Named("features.show-personal-income") showPersonalIncome: Boolean,
                                            invitationsService: InvitationsService,
                                            auditService: AuditService,
                                            val messagesApi: play.api.i18n.MessagesApi,
                                            val authConnector: AuthConnector,
                                            val withVerifiedPasscode: PasscodeVerification)
                                          (implicit val configuration: Configuration, externalUrls: ExternalUrls)
  extends FrontendController with I18nSupport with AuthActions {

  import AgentsInvitationController._

  private val personalIncomeRecord = if (showPersonalIncome)
    Seq(HMRCPIR -> Messages("select-service.personal-income-viewer")) else Seq.empty
  private val mtdItId = if (showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("select-service.itsa")) else Seq.empty

  val agentsRoot: Action[AnyContent] = ActionWithMdc { implicit request =>
    Redirect(routes.AgentsInvitationController.showNinoForm())
  }

  val showNinoForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      Future successful Ok(enter_nino(agentInvitationNinoForm))
    }
  }

  val submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationNinoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_nino(formWithErrors))
        },
        userInput => {
          Future successful Redirect(routes.AgentsInvitationController.selectService()).addingToSession("nino" -> userInput.nino.value)
        })
    }
  }

  val selectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      request.session.get("nino") match {
        case Some(nino) => Future successful Ok(select_service(agentInvitationServiceForm.fill(AgentInvitationUserInput(Nino(nino), None, None)),
          personalIncomeRecord ++ mtdItId))
        case _ => Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
      }
    }
  }

  val submitService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationServiceForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(select_service(formWithErrors, personalIncomeRecord ++ mtdItId))
        },
        userInput => {
          userInput.service match {
            case Some(HMRCMTDIT) => Future successful Redirect(routes.AgentsInvitationController.showPostcodeForm())
              .addingToSession("service" -> HMRCMTDIT)
            case Some(HMRCPIR) => createInvitation(arn, userInput)
            case _ => Future successful Ok(select_service(agentInvitationServiceForm, personalIncomeRecord ++ mtdItId))
          }
        }
      )

    }
  }

  val showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      val maybeNino = request.session.get("nino")
      val maybeService = request.session.get("service")
      (maybeNino, maybeService) match {
        case (Some(nino), Some(service)) =>
          Future successful Ok(enter_postcode(agentInvitationPostCodeForm.fill(AgentInvitationUserInput(Nino(nino), Some(service), None))))
        case (Some(nino), None) =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
        case _ =>
          Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
      }
    }
  }

  val submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationPostCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_postcode(formWithErrors))
        },
        userInput => createInvitation(arn, userInput))
    }
  }

  private def createInvitation(arn: Arn, userInput: AgentInvitationUserInput)(implicit request: Request[_]) = {
    invitationsService.createInvitation(arn, userInput)
      .map(invitation => {
        val id = invitation.selfUrl.toString.split("/").toStream.last
        if (invitation.service == HMRCMTDIT) auditService.sendAgentInvitationSubmitted(arn, id, userInput, "Success")
        else auditService.sendAgentInvitationSubmitted(arn, id, userInput, "Not Required")
        Redirect(routes.AgentsInvitationController.invitationSent())
          .addingToSession(
            "invitationId" -> id,
            "deadline" -> invitation.expiryDate.toString(DateTimeFormat.forPattern("d MMMM YYYY"))
        )
      })
      .recoverWith {
        case noMtdItId: Upstream4xxResponse if noMtdItId.message.contains("CLIENT_REGISTRATION_NOT_FOUND") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Client Registration Not Found.")
          auditService.sendAgentInvitationSubmitted(arn, "", userInput, "Fail", Some("CLIENT_REGISTRATION_NOT_FOUND"))
          Future successful Redirect(routes.AgentsInvitationController.notEnrolled())
        }
        case noPostCode: Upstream4xxResponse if noPostCode.message.contains("POSTCODE_DOES_NOT_MATCH") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
          auditService.sendAgentInvitationSubmitted(arn, "", userInput, "Fail", Some("POSTCODE_DOES_NOT_MATCH"))
          Future successful Redirect(routes.AgentsInvitationController.notMatched())
        }
        case e =>
          Logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", userInput, "Fail", Option(e.getMessage))
          Future.failed(e)
      }
  }

  val invitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      (request.session.get("invitationId"), request.session.get("deadline")) match {
        case (Some(id), Some(deadline)) =>
          val invitationUrl: String = s"$externalUrl${routes.ClientsInvitationController.start(InvitationId(id)).path()}"
          Future successful Ok(invitation_sent(invitationUrl, asAccUrl.toString, deadline))
            .removingFromSession("nino", "service", "invitationId", "deadline")
        case _ => throw new RuntimeException("User attempted to browse to invitationSent")
      }
    }
  }

  val notEnrolled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      Future successful Forbidden(not_enrolled())
    }
  }

  val notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      Future successful Forbidden(not_matched())
    }
  }
}

object AgentsInvitationController {

  private val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  private def nonEmpty(failure: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.isEmpty) Invalid(ValidationError(failure)) else Valid
  }

  private def validateField(nonEmptyFailure: String, invalidFailure: String)(condition: String => Boolean) = Constraint[String] { fieldValue: String =>
    nonEmpty(nonEmptyFailure)(fieldValue) match {
      case i: Invalid =>
        i
      case Valid =>
        if (condition(fieldValue.trim.toUpperCase))
          Valid
        else
          Invalid(ValidationError(invalidFailure))
    }
  }

  private val serviceChoice: Constraint[Option[String]] = Constraint[Option[String]] { fieldValue: Option[String] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError("error.service.required"))
  }

  private val invalidNino =
    validateField("error.nino.required", "enter-nino.invalid-format")(nino => isValid(nino))
  private val invalidPostcode =
    validateField("error.postcode.required", "enter-postcode.invalid-format")(postcode => postcode.matches(postcodeRegex))

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text.verifying(invalidNino),
      "service" -> optional(text),
      "postcode" -> optional(text))
    ({ (nino, service, postcode) => AgentInvitationUserInput(Nino(nino.trim.toUpperCase()), service, postcode) })
    ({ user => Some((user.nino.value, user.service, user.postcode)) }))
  }

  val agentInvitationServiceForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text.verifying(invalidNino),
      "service" -> optional(text).verifying(serviceChoice),
      "postcode" -> optional(text))
    ({ (nino, service, postcode) => AgentInvitationUserInput(Nino(nino.trim.toUpperCase()), service, postcode) })
    ({ user => Some((user.nino.value, user.service, user.postcode)) }))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text.verifying(invalidNino),
      "service" -> optional(text).verifying(serviceChoice),
      "postcode" -> text.verifying(invalidPostcode))
    ({ (nino, service, postcode) => AgentInvitationUserInput(Nino(nino.trim.toUpperCase()), service, Option(postcode)) })
    ({ user => Some((user.nino.value, user.service, user.postcode.getOrElse(""))) }))
  }
}

