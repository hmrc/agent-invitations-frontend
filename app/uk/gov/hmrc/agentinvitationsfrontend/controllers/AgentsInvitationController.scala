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

import javax.inject.{Inject, Named, Singleton}

import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Request}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentInvitationUserInput
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.domain.Nino.isValid
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentsInvitationController @Inject()(
                                            @Named("agent-invitations-frontend.base-url") externalUrl: String,
                                            invitationsService: InvitationsService,
                                            auditService: AuditService,
                                            val messagesApi: play.api.i18n.MessagesApi,
                                            val authConnector: AuthConnector)(implicit val configuration: Configuration)
  extends FrontendController with I18nSupport with AuthActions {

  import AgentsInvitationController._

  def agentsRoot: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.AgentsInvitationController.showNinoForm())
  }

  def showNinoForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      Future successful Ok(enter_nino(agentInvitationServiceForm))
    }
  }

  def submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationNinoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_nino(formWithErrors))
        },
        userInput => {
          Future successful Redirect(routes.AgentsInvitationController.selectService()).withSession(request.session + ("nino" -> userInput.nino.value))
        })
    }
  }

  def selectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      request.session.get("nino") match {
        case Some(nino) => Future successful Ok(select_service(agentInvitationServiceForm.fill(AgentInvitationUserInput(Nino(nino), None, ""))))
        case _ => Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
      }
    }
  }

  def submitService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationServiceForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(select_service(formWithErrors))
        },
        userInput => {
          Logger.info(s"User $arn enter its input $userInput")
          userInput.service match {
            case Some("HMRC-MTD-IT") => Future successful Redirect(routes.AgentsInvitationController.showPostcodeForm())
              .withSession(request.session + ("service" -> "HMRC-MTD-IT"))
            case Some("personal-income-record") => createInvitation(arn, userInput)
            case _ => Future successful Ok(select_service(agentInvitationServiceForm))
          }
        }
      )

    }
  }

  def showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      (request.session.get("nino"), request.session.get("service")) match {
        case (Some(nino), Some(service)) =>
          Future successful Ok(enter_postcode(agentInvitationServiceForm.fill(AgentInvitationUserInput(Nino(nino), Some(service), ""))))
        case (Some(nino), None) =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
        case _ =>
          Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
      }
    }
  }

  def submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationPostCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_postcode(formWithErrors))
        },
        userInput => createInvitation(arn, userInput))
    }
  }

  private def createInvitation(arn: Arn, userInput: AgentInvitationUserInput)(implicit request: Request[_]) = {
    invitationsService
      .createInvitation(arn, userInput)
      .map(invitation => {
        val id = extractInvitationId(invitation.selfUrl.toString)
        auditService.sendAgentInvitationSubmitted(arn, id, userInput, "Success")
        Redirect(routes.AgentsInvitationController.invitationSent).withSession(request.session + ("invitationId" -> id))
      })
      .recoverWith {
        case noMtdItId: Upstream4xxResponse if noMtdItId.message.contains("CLIENT_REGISTRATION_NOT_FOUND") => {
          auditService.sendAgentInvitationSubmitted(arn, "", userInput, "Fail", Some("CLIENT_REGISTRATION_NOT_FOUND"))
          Future successful Redirect(routes.AgentsInvitationController.notEnrolled())
        }
        case noPostCode: Upstream4xxResponse if noPostCode.message.contains("POSTCODE_DOES_NOT_MATCH") => {
          auditService.sendAgentInvitationSubmitted(arn, "", userInput, "Fail", Some("POSTCODE_DOES_NOT_MATCH"))
          Future successful Redirect(routes.AgentsInvitationController.notMatched())
        }
        case e =>
          auditService.sendAgentInvitationSubmitted(arn, "", userInput, "Fail", Option(e.getMessage))
          Future.failed(e)
      }
  }

  def invitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      request.session.get("invitationId") match {
        case Some(id) =>
          Future successful Ok(invitation_sent(s"$externalUrl${routes.ClientsInvitationController.start(InvitationId(id)).path()}"))
        case None => throw new RuntimeException("User attempted to browse to invitationSent")
      }
    }
  }

  def notEnrolled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      Future successful Forbidden(not_enrolled())
    }
  }

  def notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      Future successful Forbidden(not_matched())
    }
  }

  private def extractInvitationId(url: String) = url.substring(url.lastIndexOf("/") + 1)

}

object AgentsInvitationController {

  private def postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

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

  def serviceChoice: Constraint[Option[String]] = Constraint[Option[String]] { fieldValue: Option[String] =>
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
      "postcode" -> text)
    ({ (nino, service, postcode) => AgentInvitationUserInput(Nino(nino.trim.toUpperCase()), service, postcode) })
    ({ user => Some((user.nino.value, user.service, user.postcode)) }))
  }

  val agentInvitationServiceForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text.verifying(invalidNino),
      "service" -> optional(text).verifying(serviceChoice),
      "postcode" -> text)
    ({ (nino, service, postcode) => AgentInvitationUserInput(Nino(nino.trim.toUpperCase()), service, postcode) })
    ({ user => Some((user.nino.value, user.service, user.postcode)) }))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "nino" -> text.verifying(invalidNino),
      "service" -> optional(text).verifying(serviceChoice),
      "postcode" -> text.verifying(invalidPostcode))
    ({ (nino, service, postcode) => AgentInvitationUserInput(Nino(nino.trim.toUpperCase()), service, postcode) })
    ({ user => Some((user.nino.value, user.service, user.postcode)) }))
  }
}
