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
import org.joda.time.format.DateTimeFormat
import play.api.data.Forms._
import play.api.data.validation._
import play.api.data.{Form, Mapping}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request}
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitationUserInput, AgentInvitationVatForm}
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.Upstream4xxResponse
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

@Singleton
class AgentsInvitationController @Inject()(
                                            @Named("agent-invitations-frontend.external-url") externalUrl: String,
                                            @Named("agent-services-account-frontend.external-url") asAccUrl: String,
                                            featureFlags: FeatureFlags,
                                            invitationsService: InvitationsService,
                                            auditService: AuditService,
                                            val messagesApi: play.api.i18n.MessagesApi,
                                            val authConnector: AuthConnector,
                                            val withVerifiedPasscode: PasscodeVerification)
                                          (implicit val configuration: Configuration, val externalUrls: ExternalUrls)
  extends FrontendController with I18nSupport with AuthActions {

  import AgentsInvitationController._

  private val personalIncomeRecord = if (featureFlags.showPersonalIncome)
    Seq(HMRCPIR -> Messages("select-service.personal-income-viewer")) else Seq.empty
  private val mtdItId = if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("select-service.itsa")) else Seq.empty
  private val vat = if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  private def enabledServices(isWhitelisted:Boolean): Seq[(String,String)] = {
    if(isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }
  }

  val agentsRoot: Action[AnyContent] = ActionWithMdc { implicit request =>
    Redirect(routes.AgentsInvitationController.selectService())
  }

  val showNinoForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      request.session.get("service") match {
        case Some(service) => Future successful Ok (enter_nino (agentInvitationNinoForm.fill(AgentInvitationUserInput(service, None, None))))
        case None => Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }


  val submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      agentInvitationNinoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_nino(formWithErrors))
        },
        userInput => {
          (userInput,featureFlags) match {
            case ClientForMtdItWithFlagOn(clientIdentifier) =>
              Future successful Redirect(routes.AgentsInvitationController.showPostcodeForm())
                .addingToSession("clientIdentifier" -> clientIdentifier.value)

            case ClientForPirWithFlagOn(_) =>
              throw new Exception("KFC flagged as on, not implemented for personal-income-record")

            case ClientWithItsaOrPirFlagOff(_) =>
              createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, None)

            case _ => Future successful Ok(enter_nino(agentInvitationNinoForm))
          }
        })
    }
  }

  val showVrnForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      request.session.get("service") match {
        case Some(service) => Future successful Ok(enter_vrn(agentInvitationVrnForm.fill(AgentInvitationVatForm(service, None, None))))
        case _ => Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitVrn: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, _) =>
      agentInvitationVrnForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_vrn(formWithErrors))
        },
        userInput => {
          (userInput, featureFlags) match {
            case ClientForVatWithFlagOn(clientIdentifier) =>
              Future successful Redirect(routes.AgentsInvitationController.showVatRegistrationDateForm())
                .addingToSession("clientIdentifier" -> clientIdentifier.value)

            case ClientWithVatFlagOff(_) =>
              createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, None)

            case _ => Future successful Ok(enter_vrn(agentInvitationVrnForm))
          }
        }
      )
    }
  }

  val showVatRegistrationDateForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
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
    withAuthorisedAsAgent { (arn, _) =>
      agentInvitationVatRegistrationDateForm.bindFromRequest().fold(
        formWithErrors => Future successful Ok(enter_vat_registration_date(formWithErrors)),
        userInput => {
          val suppliedVrn = userInput.clientIdentifier
            .map(_.value)
            .map(Vrn.apply)
            .getOrElse(throw new IllegalStateException(s"ClientIdentifier missing. form data: $userInput"))
          val suppliedVatRegDate = LocalDate.parse(userInput.registrationDate.getOrElse(""))

          invitationsService.checkVatRegistrationDateMatches(suppliedVrn, suppliedVatRegDate) flatMap {
            case Some(true) => createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, None)
            case Some(false) => Future successful Redirect(routes.AgentsInvitationController.notMatched())
            case None => Future successful Redirect(routes.AgentsInvitationController.notEnrolled())
          }
        }
      )
    }
  }

  val selectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      Future successful Ok(select_service(agentInvitationServiceForm, enabledServices(isWhitelisted)))
    }
  }

  val submitService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      val allowedServices = enabledServices(isWhitelisted)
      agentInvitationServiceForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(select_service(formWithErrors, allowedServices))
        },
        userInput => {
          userInput.service match {
            case HMRCMTDVAT => Future successful Redirect(routes.AgentsInvitationController.showVrnForm())
              .addingToSession("service" -> HMRCMTDVAT)
            case service if allowedServices.exists(_._1 == service) => Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
              .addingToSession("service" -> service)
            case _ => Future successful BadRequest
          }
        }
      )
    }
  }

  val showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
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
    withAuthorisedAsAgent { (arn, _) =>
      agentInvitationPostCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_postcode(formWithErrors))
        },
        userInput => createInvitation(arn, userInput.service, userInput.clientIdentifierType, userInput.clientIdentifier, userInput.postcode))
    }
  }

  private def createInvitation(arn: Arn, service: String, clientIdentifierType: Option[String], clientIdentifier: Option[TaxIdentifier], postcode: Option[String])(implicit request: Request[_]) = {
    invitationsService.createInvitation(arn, service, clientIdentifierType, clientIdentifier, postcode)
      .map(invitation => {
        val id = invitation.selfUrl.toString.split("/").toStream.last
        if ((invitation.service == HMRCMTDIT && featureFlags.showKfcMtdIt)
          | (invitation.service == HMRCPIR && featureFlags.showKfcPersonalIncome)
          | (invitation.service == HMRCMTDVAT && featureFlags.showKfcMtdVat)) {
          auditService.sendAgentInvitationSubmitted(arn, id, service, clientIdentifierType, clientIdentifier, "Success")
        }
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
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
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
    withAuthorisedAsAgent { (_, _) =>
      request.session.get("service") match {
        case Some(HMRCMTDVAT) =>
          Future successful Forbidden(not_enrolled(Messages("not-enrolled.vat.header"), Messages("not-enrolled.vat.description")))
        case Some(HMRCMTDIT) =>
          Future successful Forbidden(not_enrolled(Messages("not-enrolled.itsa.header"), Messages("not-enrolled.itsa.description")))
        case _ =>
          Future failed(throw new Exception("Unsupported Service"))
      }
    }
  }

  val notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
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

  private def validateVrnField(nonEmptyFailure: String,
                               regexFailure: String,
                               checksumFailure: String) = Constraint[String] { fieldValue: String =>
    nonEmpty(nonEmptyFailure)(fieldValue) match {
      case i: Invalid =>
        i
      case Valid =>
        if(!fieldValue.matches("[0-9]{9}"))
          Invalid(ValidationError(regexFailure))
        else if(!Vrn.isValid(fieldValue.trim.toUpperCase))
          Invalid(ValidationError(checksumFailure))
        else
          Valid
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
    validateVrnField("error.vrn.required", "enter-vrn.regex-failure", "enter-vrn.checksum-failure")
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
      case _ : Throwable => false
    }
  }

  private val invalidPostcode =
    validateField("error.postcode.required", "enter-postcode.invalid-format")(postcode => postcode.matches(postcodeRegex))

  import play.api.data.format.Formats.stringFormat
  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> normalizedText.verifying(invalidNino),
      "postcode" -> optional(text))
    ({ (service, clientIdentifier, _) => AgentInvitationUserInput(service, Some(Nino(clientIdentifier.trim.toUpperCase())), None) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), None)) }))
  }

  val agentInvitationVrnForm: Form[AgentInvitationVatForm] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> normalizedText.verifying(invalidVrn),
      "registrationDate" -> optional(text))
    ({ (service, clientIdentifier, _) => AgentInvitationVatForm(service, Some(Vrn(clientIdentifier)), None) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.registrationDate))}))
  }

  val agentInvitationVatRegistrationDateForm: Form[AgentInvitationVatForm] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> normalizedText.verifying(invalidVrn),
      "registrationDate" -> text.verifying(invalidVatDateFormat))
    ({ (service, clientIdentifier, registrationDate) => AgentInvitationVatForm(service, Some(Vrn(clientIdentifier)), Some(registrationDate)) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.registrationDate.getOrElse(""))) }))
  }

  val agentInvitationServiceForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> optional(normalizedText),
      "postcode" -> optional(text))
    ({ (service, _, _) => AgentInvitationUserInput(service, None, None) })
    ({ user => Some((user.service, None, None)) }))
  }

  val agentInvitationPostCodeForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> normalizedText.verifying(invalidNino),
      "postcode" -> text.verifying(invalidPostcode))
    ({ (service, nino, postcode) => AgentInvitationUserInput(service, Some(Nino(nino.trim.toUpperCase())), Some(postcode)) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.postcode.getOrElse(""))) }))
  }

  object ClientForMtdItWithFlagOn {
    def unapply(arg: (AgentInvitationUserInput, FeatureFlags)): Option[TaxIdentifier] = arg match {
      case (AgentInvitationUserInput(HMRCMTDIT, Some(clientIdentifier),_), featureFlags) if featureFlags.showKfcMtdIt =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientForPirWithFlagOn {
    def unapply(arg: (AgentInvitationUserInput, FeatureFlags)): Option[Unit] = arg match {
      case (AgentInvitationUserInput(HMRCPIR, Some(_), _), featureFlags) if featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientWithItsaOrPirFlagOff {
    def unapply(arg: (AgentInvitationUserInput, FeatureFlags)): Option[Unit] = arg match {
      case (AgentInvitationUserInput(_, Some(_), _), featureFlags) if !featureFlags.showKfcMtdIt || !featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientForVatWithFlagOn {
    def unapply(arg: (AgentInvitationVatForm, FeatureFlags)) : Option[TaxIdentifier] = arg match {
      case (AgentInvitationVatForm(HMRCMTDVAT, Some(clientIdentifier), _), featureFlags) if featureFlags.showKfcMtdVat =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientWithVatFlagOff {
    def unapply(arg: (AgentInvitationVatForm, FeatureFlags)): Option[Unit] = arg match {
      case (AgentInvitationVatForm(_, Some(_), _), featureFlags) if !featureFlags.showKfcMtdVat =>
        Some(())
      case _ => None
    }
  }

}

