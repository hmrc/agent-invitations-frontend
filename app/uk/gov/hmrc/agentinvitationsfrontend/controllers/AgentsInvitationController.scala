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
import uk.gov.hmrc.domain.{Nino, Vrn}
import uk.gov.hmrc.domain.Nino.isValid
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}

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
  private val vat = if (true) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  val agentsRoot: Action[AnyContent] = ActionWithMdc { implicit request =>
    Redirect(routes.AgentsInvitationController.selectService())
  }

  val showNinoForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      request.session.get("service") match {
        case Some(service) => Future successful Ok (enter_nino (agentInvitationNinoForm.fill(AgentInvitationUserInput(service, None, None))))
        case None => Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationNinoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_nino(formWithErrors))
        },
        userInput => {
          userInput.taxIdentifier match {
            case Some(taxIdentifier) if userInput.service == HMRCMTDIT => Future successful Redirect(routes.AgentsInvitationController.showPostcodeForm())
              .addingToSession("taxIdentifier" -> taxIdentifier.value)
            case Some(_) if userInput.service == HMRCPIR => createInvitation(arn, userInput)
            case _ => Future successful Ok(enter_nino(agentInvitationNinoForm))
          }
        })
    }
  }

  val showVrnForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      request.session.get("service") match {
        case Some(_) => Future successful Ok(enter_vrn(agentInvitationVrnForm))
        case _ => Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitVrn: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationVrnForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_vrn(formWithErrors))
        },
        userInput => createInvitation(arn, userInput))
    }
  }

  val selectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      Future successful Ok(select_service(agentInvitationServiceForm, personalIncomeRecord ++ mtdItId ++ vat))
    }
  }

  val submitService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationServiceForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(select_service(formWithErrors, personalIncomeRecord ++ mtdItId ++ vat))
        },
        userInput => {
          userInput.service match {
            case HMRCMTDVAT => Future successful Redirect(routes.AgentsInvitationController.showVrnForm())
            case service => Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
              .addingToSession("service" -> service)
            case _ => Future successful Ok(select_service(agentInvitationServiceForm, personalIncomeRecord ++ mtdItId ++ vat))
          }
        }
      )
    }
  }

  val showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      val maybeNino = request.session.get("taxIdentifier")
      val maybeService = request.session.get("service")
      (maybeNino, maybeService) match {
        case (Some(nino), Some(service)) =>
          Future successful Ok(enter_postcode(agentInvitationPostCodeForm.fill(AgentInvitationUserInput(service, Some(Nino(nino)), None))))
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
      Logger.info(s"Session contains ${request.session.get("invitationId")} ${request.session.get("deadline")}")
      (request.session.get("invitationId"), request.session.get("deadline")) match {
        case (Some(id), Some(deadline)) =>
          val invitationUrl: String = s"$externalUrl${routes.ClientsInvitationController.start(InvitationId(id)).path()}"
          Future successful Ok(invitation_sent(invitationUrl, asAccUrl.toString, deadline))
            .removingFromSession("taxIdentifier", "service", "invitationId", "deadline")
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

  private val serviceChoice: Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError("error.service.required"))
  }

  private val vrnRegex =  "^(GB)?([0-9]{9}([0-9]{3})?|[A-Z]{2}[0-9]{3})$"
  private def isValidVrn(vrn: String): Boolean = vrn.matches(vrnRegex)

  private val invalidNino =
    validateField("error.nino.required", "enter-nino.invalid-format")(nino => isValid(nino))
  private val invalidVrn =
    validateField("error.vrn.required", "enter-vrn.invalid-format")(vrn => isValidVrn(vrn))
  private val invalidPostcode =
    validateField("error.postcode.required", "enter-postcode.invalid-format")(postcode => postcode.matches(postcodeRegex))

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "taxIdentifier" -> text.verifying(invalidNino),
      "postcode" -> optional(text))
    ({ (service, taxIdentifier, postcode) => AgentInvitationUserInput(service, Some(Nino(taxIdentifier.trim.toUpperCase())), postcode) })
    ({ user => Some((user.service, user.taxIdentifier.map(_.value).getOrElse(""), user.postcode)) }))
  }

  val agentInvitationVrnForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "taxIdentifier" -> text.verifying(invalidVrn),
      "postcode" -> optional(text))
    ({ (service, taxIdentifier, postcode) => AgentInvitationUserInput(service, Some(Vrn(taxIdentifier)), postcode) })
    ({ user => Some((user.service, user.taxIdentifier.map(_.value).getOrElse(""), user.postcode)) }))
  }

  val agentInvitationServiceForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "taxIdentifier" -> optional(text),
      "postcode" -> optional(text))
    ({ (service, taxIdentifier, postcode) => AgentInvitationUserInput(service, taxIdentifier.map(x => Nino(x.toUpperCase)), postcode) })
    ({ user => Some((user.service, user.taxIdentifier.map(_.value), user.postcode)) }))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "taxIdentifier" -> text.verifying(invalidNino),
      "postcode" -> text.verifying(invalidPostcode))
    ({ (service, nino, postcode) => AgentInvitationUserInput(service, Some(Nino(nino.trim.toUpperCase())), Some(postcode)) })
    ({ user => Some((user.service, user.taxIdentifier.map(_.value).getOrElse(""), user.postcode.getOrElse(""))) }))
  }
}

