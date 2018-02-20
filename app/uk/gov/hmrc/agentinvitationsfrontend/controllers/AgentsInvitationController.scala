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
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation._
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitationForm, AgentInvitationUserInput, AgentInvitationVatForm}
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

@Singleton
class AgentsInvitationController @Inject()(
                                            @Named("agent-invitations-frontend.external-url") externalUrl: String,
                                            @Named("agent-services-account-frontend.external-url") asAccUrl: String,
                                            @Named("features.show-hmrc-mtd-it") showHmrcMtdIt: Boolean,
                                            @Named("features.show-personal-income") showPersonalIncome: Boolean,
                                            @Named("features.show-hmrc-mtd-vat") showHmrcMtdVat: Boolean,
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
  private val vat = if (showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  private val enabledServices = personalIncomeRecord ++ mtdItId ++ vat

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
          userInput.clientIdentifier match {
            case Some(clientIdentifier) if userInput.service == HMRCMTDIT => Future successful Redirect(routes.AgentsInvitationController.showPostcodeForm())
              .addingToSession("clientIdentifier" -> clientIdentifier.value)
            case Some(_) if userInput.service == HMRCPIR => createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, userInput.postcode)
            case _ => Future successful Ok(enter_nino(agentInvitationNinoForm))
          }
        })
    }
  }

  val showVrnForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      request.session.get("service") match {
        case Some(service) => Future successful Ok(enter_vrn(agentInvitationVrnForm.fill(AgentInvitationVatForm(service, None, None))))
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
        userInput => {
          userInput.clientIdentifier match {
            case Some(clientIdentifier) =>
              Future successful Redirect(routes.AgentsInvitationController.showVatRegistrationDateForm())
                .addingToSession("clientIdentifier" -> clientIdentifier.value)
            case _ => Future successful Ok(enter_vrn(agentInvitationVrnForm))
          }
        }
      )
    }
  }

  val showVatRegistrationDateForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      val maybeVrn = request.session.get("clientIdentifier")
      val maybeService = request.session.get("service")
      (maybeVrn, maybeService) match {
        case (Some(vrn), Some(service)) =>
          Future successful
            Ok(enter_vat_registration_date(agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(service, Some(Vrn(vrn)), None))))
        case (None, Some(service)) =>
          Future successful Redirect(routes.AgentsInvitationController.showVrnForm()).addingToSession("service" -> service)
        case _ =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitVatRegistrationDate: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationVatRegistrationDateForm.bindFromRequest().fold(
        formWithErrors => Future successful Ok(enter_vat_registration_date(formWithErrors)),
        userInput => {
          checkVatRegistrationDateMatches(userInput.clientIdentifier
            .getOrElse(throw new IllegalStateException(s"ClientIdentifier missing. form data: $userInput")).value,
            userInput.registrationDate.getOrElse("")) flatMap {
            case (true, true) => createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, None)
            case (true, false) => Future successful Redirect(routes.AgentsInvitationController.notMatched())
            case (false, false) => Future successful Redirect(routes.AgentsInvitationController.notEnrolled())
          }
        }
      )
    }
  }

  val selectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      Future successful Ok(select_service(agentInvitationServiceForm, enabledServices))
    }
  }

  val submitService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      agentInvitationServiceForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(select_service(formWithErrors, enabledServices))
        },
        userInput => {
          userInput.service match {
            case HMRCMTDVAT => Future successful Redirect(routes.AgentsInvitationController.showVrnForm())
              .addingToSession("service" -> HMRCMTDVAT)
            case service => Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
              .addingToSession("service" -> service)
            case _ => Future successful Ok(select_service(agentInvitationServiceForm, enabledServices))
          }
        }
      )
    }
  }

  val showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { arn =>
      val maybeNino = request.session.get("clientIdentifier")
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
        userInput => createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, userInput.postcode))
    }
  }

  private def checkVatRegistrationDateMatches(vrn: String, vatRegistrationDate: String) =
    invitationsService.checkVatRegistrationDateMatches(vrn, vatRegistrationDate)

  private def createInvitation(arn: Arn, service: String, clientIdentifierType: Option[String], clientIdentifier: Option[TaxIdentifier], postcode: Option[String])(implicit request: Request[_]) = {
    invitationsService.createInvitation(arn, service, clientIdentifierType, clientIdentifier, postcode)
      .map(invitation => {
        val id = invitation.selfUrl.toString.split("/").toStream.last
        if (invitation.service == HMRCMTDIT) auditService.sendAgentInvitationSubmitted(arn, id, service, clientIdentifierType, clientIdentifier, "Success")
        else auditService.sendAgentInvitationSubmitted(arn, id, service, clientIdentifierType, clientIdentifier, "Not Required")
        Redirect(routes.AgentsInvitationController.invitationSent())
          .addingToSession(
            "invitationId" -> id,
            "deadline" -> invitation.expiryDate.toString(DateTimeFormat.forPattern("d MMMM YYYY"))
        )
      })
      .recoverWith {
        case noMtdItId: Upstream4xxResponse if noMtdItId.message.contains("CLIENT_REGISTRATION_NOT_FOUND") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Client Registration Not Found.")
          auditService.sendAgentInvitationSubmitted(arn, "", service, clientIdentifierType, clientIdentifier, "Fail", Some("CLIENT_REGISTRATION_NOT_FOUND"))
          Future successful Redirect(routes.AgentsInvitationController.notEnrolled())
        }
        case noPostCode: Upstream4xxResponse if noPostCode.message.contains("POSTCODE_DOES_NOT_MATCH") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
          auditService.sendAgentInvitationSubmitted(arn, "", service, clientIdentifierType, clientIdentifier, "Fail", Some("POSTCODE_DOES_NOT_MATCH"))
          Future successful Redirect(routes.AgentsInvitationController.notMatched())
        }
        case e =>
          Logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", service, clientIdentifierType, clientIdentifier, "Fail", Option(e.getMessage))
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
            .removingFromSession("clientIdentifier", "service", "invitationId", "deadline")
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

  private val invalidNino =
    validateField("error.nino.required", "enter-nino.invalid-format")(nino => Nino.isValid(nino))
  private val invalidVrn =
    validateField("error.vrn.required", "enter-vrn.invalid-format")(vrn => Vrn.isValid(vrn))
  private val invalidVatDateFormat =
    validateField("error.vat-registration-date.required", "enter-vat-registration-date.invalid-format")(vatRegistrationDate => validateDate(vatRegistrationDate))

  private def validateDate(value: String): Boolean =  if (parseDate(value)) true else false

  private def parseDate(date: String): Boolean = {
    import org.joda.time.format.DateTimeFormat
    try {
      DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(date)
      true
    }
    catch {
      case _ @ _ => false
    }
  }
  private val invalidPostcode =
    validateField("error.postcode.required", "enter-postcode.invalid-format")(postcode => postcode.matches(postcodeRegex))

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> text.verifying(invalidNino),
      "postcode" -> optional(text))
    ({ (service, clientIdentifier, _) => AgentInvitationUserInput(service, Some(Nino(clientIdentifier.trim.toUpperCase())), None) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), None)) }))
  }

  val agentInvitationVrnForm: Form[AgentInvitationVatForm] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> text.verifying(invalidVrn),
      "registrationDate" -> optional(text))
    ({ (service, clientIdentifier, _) => AgentInvitationVatForm(service, Some(Vrn(clientIdentifier)), None) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.registrationDate))}))
  }

  val agentInvitationVatRegistrationDateForm: Form[AgentInvitationVatForm] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> text.verifying(invalidVrn),
      "registrationDate" -> text.verifying(invalidVatDateFormat))
    ({ (service, clientIdentifier, registrationDate) => AgentInvitationVatForm(service, Some(Vrn(clientIdentifier)), Some(registrationDate)) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.registrationDate.getOrElse(""))) }))
  }

  val agentInvitationServiceForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> optional(text),
      "postcode" -> optional(text))
    ({ (service, _, _) => AgentInvitationUserInput(service, None, None) })
    ({ user => Some((user.service, None, None)) }))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> text.verifying(invalidNino),
      "postcode" -> text.verifying(invalidPostcode))
    ({ (service, nino, postcode) => AgentInvitationUserInput(service, Some(Nino(nino.trim.toUpperCase())), Some(postcode)) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.postcode.getOrElse(""))) }))
  }
}

