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
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.services.InvitationsService
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.{HeaderCarrier, Upstream4xxResponse}
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

@Singleton
class AgentsInvitationController @Inject()(@Named("agent-invitations-frontend.external-url") externalUrl: String,
                                           @Named("agent-services-account-frontend.external-url") asAccUrl: String,
                                           featureFlags: FeatureFlags,
                                           invitationsService: InvitationsService,
                                           auditService: AuditService,
                                           fastTrackCache: FastTrackKeyStoreCache,
                                           continueUrlStoreService: ContinueUrlStoreService,
                                           val messagesApi: play.api.i18n.MessagesApi,
                                           val env: Environment,
                                           val authConnector: AuthConnector,
                                           val continueUrlActions: ContinueUrlActions,
                                           val withVerifiedPasscode: PasscodeVerification)
                                          (implicit val configuration: Configuration, val externalUrls: ExternalUrls)
  extends FrontendController with I18nSupport with AuthActions {

  import AgentsInvitationController._
  import continueUrlActions._

  private val personalIncomeRecord = if (featureFlags.showPersonalIncome)
    Seq(HMRCPIR -> Messages("select-service.personal-income-viewer")) else Seq.empty
  private val mtdItId = if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("select-service.itsa")) else Seq.empty
  private val vat = if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT -> Messages("select-service.vat")) else Seq.empty

  private def enabledServices(isWhitelisted: Boolean): Seq[(String, String)] = {
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }
  }

  private[controllers] val isDevEnv = if (env.mode.equals(Mode.Test)) false else configuration.getString("run.mode").forall(Mode.Dev.toString.equals)
  private[controllers] val agentServicesAccountUrl: String = if(isDevEnv) s"http://localhost:9401/agent-services-account" else "/agent-services-account"

  val agentsRoot: Action[AnyContent] = ActionWithMdc { implicit request =>
    Redirect(routes.AgentsInvitationController.selectService())
  }

  val selectService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, isWhitelisted) =>
      Future successful Ok(select_service(agentInvitationServiceForm, enabledServices(isWhitelisted)))
    }
  }

  val submitService: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      val allowedServices = enabledServices(isWhitelisted)
      agentInvitationServiceForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(select_service(formWithErrors, allowedServices))
        },
        userInput => {
          val updateAggregate = fastTrackCache.fetchAndGetEntry()
            .map(_.getOrElse(FastTrackInvitation.newInstance))
            .map(_.copy(service = Some(userInput.service)))

          updateAggregate.flatMap(updateFastTrack =>
            fastTrackCache.save(updateFastTrack).flatMap(_ =>
              ifShouldShowService(updateFastTrack, featureFlags, isWhitelisted) {
                redirectFastTrack(arn, updateFastTrack, isWhitelisted)
              }
            ))
        }
      )
    }
  }

  val showNinoForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetchAndGetEntry().flatMap {
        case Some(aggregate) =>
          val service = aggregate.service.getOrElse("")
          Future successful Ok(enter_nino(agentInvitationNinoForm.fill(AgentInvitationUserInput(service, None, aggregate.postcode))))
        case None =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }


  val submitNino: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentInvitationNinoForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_nino(formWithErrors))
        },
        userInput => {
          val updatedAggregate = fastTrackCache.fetchAndGetEntry()
            .map(_.getOrElse(FastTrackInvitation.newInstance))
            .map(_.copy(clientIdentifier = userInput.clientIdentifier.map(nino => nino.value)))

          updatedAggregate.flatMap(updatedInvitation =>
            fastTrackCache.save(updatedInvitation).flatMap { _ =>
              redirectFastTrack(arn, updatedInvitation, isWhitelisted)
            })
        })
    }
  }

  val showVrnForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetchAndGetEntry().flatMap {
        case Some(aggregate) =>
          val service = aggregate.service.getOrElse("")
          Future successful Ok(enter_vrn(agentInvitationVrnForm.fill(AgentInvitationVatForm(service, None, aggregate.vatRegDate))))
        case None =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitVrn: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentInvitationVrnForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_vrn(formWithErrors))
        },
        userInput => {
          val updatedAggregate = fastTrackCache.fetchAndGetEntry()
            .map(_.getOrElse(FastTrackInvitation.newInstance))
            .map(_.copy(clientIdentifier = userInput.clientIdentifier.map(vrn => vrn.value)))

          updatedAggregate.flatMap(updatedInvitation =>
            fastTrackCache.save(updatedInvitation).flatMap(_ =>
              redirectFastTrack(arn, updatedInvitation, isWhitelisted)))
        }
      )
    }
  }

  val showVatRegistrationDateForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetchAndGetEntry().flatMap {
        case Some(aggregate) =>
          val service = aggregate.service.getOrElse("")
          val vrn = aggregate.clientIdentifier.map(vrn => Vrn(vrn))
          Future successful Ok(enter_vat_registration_date(agentInvitationVatRegistrationDateForm.fill(AgentInvitationVatForm(service, vrn, None))))
        case None =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitVatRegistrationDate: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentInvitationVatRegistrationDateForm.bindFromRequest().fold(
        formWithErrors => Future successful Ok(enter_vat_registration_date(formWithErrors)),
        userInput => {
          val updatedAggregate = fastTrackCache.fetchAndGetEntry()
            .map(_.getOrElse(FastTrackInvitation.newInstance))
            .map(_.copy(vatRegDate = userInput.registrationDate))

          updatedAggregate.flatMap(updatedInvitation =>
            fastTrackCache.save(updatedInvitation).flatMap(_ =>
              redirectFastTrack(arn, updatedInvitation, isWhitelisted)))
        })
    }
  }

  val showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetchAndGetEntry().flatMap {
        case Some(aggregate) =>
          val service = aggregate.service.getOrElse("")
          val nino = aggregate.clientIdentifier.map(nino => Nino(nino))
          Future successful Ok(enter_postcode(agentInvitationPostCodeForm.fill(AgentInvitationUserInput(service, nino, None))))
        case None => Future successful Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitPostcode: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      agentInvitationPostCodeForm.bindFromRequest().fold(
        formWithErrors => {
          Future successful Ok(enter_postcode(formWithErrors))
        },
        userInput => {
          val updatedAggregate = fastTrackCache.fetchAndGetEntry()
            .map(_.getOrElse(FastTrackInvitation.newInstance))
            .map(_.copy(postcode = userInput.postcode))

          updatedAggregate.flatMap(updatedInvitation =>
            fastTrackCache.save(updatedInvitation).flatMap(_ =>
              redirectFastTrack(arn, updatedInvitation, isWhitelisted)))
        })
    }
  }

  private[controllers] def createInvitation(arn: Arn, service: String, clientIdentifierType: Option[String], clientIdentifier: Option[TaxIdentifier], postcode: Option[String])(implicit request: Request[_]) = {
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
          fastTrackCache.save(FastTrackInvitation.newInstance.copy(service = Some(service))).map { _ =>
            Redirect(routes.AgentsInvitationController.notEnrolled())
          }
        }
        case noPostCode: Upstream4xxResponse if noPostCode.message.contains("POSTCODE_DOES_NOT_MATCH") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
          auditService.sendAgentInvitationSubmitted(arn, "", service, clientIdentifierType, clientIdentifier, "Fail", Some("POSTCODE_DOES_NOT_MATCH"))
          fastTrackCache.save(FastTrackInvitation.newInstance.copy(service = Some(service))).map { _ =>
            Redirect(routes.AgentsInvitationController.notMatched())
          }
        }
        case e =>
          Logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", service, clientIdentifierType, clientIdentifier, "Fail", Option(e.getMessage))
          Future.failed(e)
      }

  }

  val invitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Logger.info(s"Session contains ${request.session.get("invitationId")} ${request.session.get("deadline")}")
      (request.session.get("invitationId"), request.session.get("deadline")) match {
        case (Some(id), Some(deadline)) =>
          val invitationUrl: String = s"$externalUrl${routes.ClientsInvitationController.start(InvitationId(id)).path()}"
          for {
            _ <- fastTrackCache.save(FastTrackInvitation.newInstance)
            continue <- continueUrlStoreService.fetchContinueUrl
          } yield Ok(invitation_sent(invitationUrl, deadline, continue.isDefined))
        case _ =>
          throw new RuntimeException("User attempted to browse to invitationSent")
      }
    }
  }

  val continueAfterInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      for {
        continue <- continueUrlStoreService.fetchContinueUrl.map(continue => continue.getOrElse(ContinueUrl(agentServicesAccountUrl)))
        _ <- continueUrlStoreService.remove()
      } yield Redirect(continue.url)
    }
  }

  val notEnrolled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetchAndGetEntry().map {
        case Some(aggregate) => aggregate.service match {
          case Some(HMRCMTDVAT) =>
            Forbidden(not_enrolled(Messages("not-enrolled.vat.header"), Messages("not-enrolled.vat.description")))
          case Some(HMRCMTDIT) =>
            Forbidden(not_enrolled(Messages("not-enrolled.itsa.header"), Messages("not-enrolled.itsa.description")))
          case _ =>
            throw new Exception("Unsupported Service")
        }
        case None => throw new Exception("Empty Cache")
      }
    }
  }

  val notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Future successful Forbidden(not_matched())
    }
  }

  val agentFastTrack: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      if(featureFlags.enableFastTrack){
        agentFastTrackForm.bindFromRequest().fold(
          _ => Future successful Redirect(routes.AgentsInvitationController.selectService()),
          fastTrackInvitation => {
            fastTrackCache.save(fastTrackInvitation).flatMap { _ =>
              withMaybeContinueUrlCached {
                ifShouldShowService(fastTrackInvitation, featureFlags, isWhitelisted) {
                  redirectFastTrack(arn, fastTrackInvitation, isWhitelisted)
                }
              }
            }
          })
      } else{
        Logger.info("Fast-Track feature flag is switched off")
        Future successful BadRequest
      }
    }
  }

  private[controllers] def redirectFastTrack(arn: Arn, fastTrackInvitation: FastTrackInvitation, isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] = {
    val updatedInvitationWithClientType = fastTrackInvitation.copy(clientIdentifierType = fastTrackInvitation.clientIdentifierTypeConversion)
    updatedInvitationWithClientType match {
      case FastTrackInvitationVatComplete(completeVatInvitation) =>
        validateRegDateAndCreate(completeVatInvitation, arn)

      case FastTrackInvitationItsaComplete(completeItsaInvitation) =>
        val ninoOpt = completeItsaInvitation.clientIdentifier.map(nino => Nino(nino))
        createInvitation(arn, HMRCMTDIT, completeItsaInvitation.clientIdentifierType, ninoOpt, completeItsaInvitation.postcode)

      case FastTrackInvitationIrvComplete(completeIRVInvitation) =>
        val ninoOpt = completeIRVInvitation.clientIdentifier.map(nino => Nino(nino))
        if (featureFlags.showKfcPersonalIncome)
          throw new Exception("KFC flagged as on, not implemented for personal-income-record")
        else
          createInvitation(arn, HMRCPIR, completeIRVInvitation.clientIdentifierType, ninoOpt, completeIRVInvitation.postcode)

      case FastTrackInvitationNeedsClientIdentifier(invitationNeedsClientIdentifier) => invitationNeedsClientIdentifier.service match {
        case Some(HMRCMTDVAT) =>
          Future successful Redirect(routes.AgentsInvitationController.showVrnForm())
        case Some(service) if isSupportedWhitelistedService(service, isWhitelisted) =>
          Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
        case _ =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
      }

      case FastTrackInvitationNeedsKnownFact(invitationNeedsKnownFact) =>
        (invitationNeedsKnownFact.service, invitationNeedsKnownFact.clientIdentifier) match {
          case (Some(HMRCMTDVAT), Some(_)) =>
            if (featureFlags.showKfcMtdVat)
              Future successful Redirect(routes.AgentsInvitationController.showVatRegistrationDateForm())
            else
              createInvitation(arn, HMRCMTDVAT, invitationNeedsKnownFact.clientIdentifierType, invitationNeedsKnownFact.clientIdentifier.map(Vrn(_)), None)

          case (Some(HMRCMTDIT), Some(clientId)) if Nino.isValid(clientId) =>
            if (featureFlags.showKfcMtdIt) Future successful Redirect(routes.AgentsInvitationController.showPostcodeForm())
            else createInvitation(arn, HMRCMTDIT, invitationNeedsKnownFact.clientIdentifierType, invitationNeedsKnownFact.clientIdentifier.map(Nino(_)), None)

          case _ =>
            Future successful Redirect(routes.AgentsInvitationController.selectService())
        }
      case FastTrackInvitationNeedsService(_) =>
        Future successful Redirect(routes.AgentsInvitationController.selectService())

      case _ =>
        Logger.warn("Resetting due to mix data in session")
        fastTrackCache.save(FastTrackInvitation.newInstance).map(_ =>
          Redirect(routes.AgentsInvitationController.selectService()))
    }
  }

  private def ifShouldShowService(fastTrackInvitation: FastTrackInvitation, featureFlags: FeatureFlags, isWhitelisted: Boolean)
                               (body: => Future[Result]): Future[Result] = {
    fastTrackInvitation.service match {
      case Some(HMRCPIR) if !isWhitelisted =>
        Logger.warn(s"User is not whitelisted to create $HMRCPIR invitation")
        Future successful BadRequest
      case Some(HMRCMTDVAT) if !featureFlags.showHmrcMtdVat =>
        Logger.warn(s"Service: $HMRCMTDVAT feature flagged is switched off")
        Future successful BadRequest
      case Some(HMRCMTDIT) if !featureFlags.showHmrcMtdIt =>
        Logger.warn(s"Service: $HMRCMTDIT feature flagged is switched off")
        Future successful BadRequest
      case Some(HMRCPIR) if !featureFlags.showPersonalIncome =>
        Logger.warn(s"Service: $HMRCPIR feature flagged is switched off")
        Future successful BadRequest
      case _ => body
    }
  }

  private def isSupportedWhitelistedService(service: String, isWhitelisted: Boolean): Boolean =
    enabledServices(isWhitelisted).exists(_._1 == service)

  private def validateRegDateAndCreate(fastTrackInvitation: FastTrackInvitation, arn: Arn)(implicit request: Request[_], hc: HeaderCarrier) = {
    val suppliedVrn = fastTrackInvitation.clientIdentifier
      .map(Vrn.apply)
      .getOrElse(throw new IllegalStateException(s"ClientIdentifier missing. form data: $fastTrackInvitation"))
    val suppliedVatRegDate = LocalDate.parse(fastTrackInvitation.vatRegDate
      .getOrElse(throw new IllegalStateException(s"VatRegDate missing. form data: $fastTrackInvitation")))
    val service = fastTrackInvitation.service
      .getOrElse(throw new IllegalStateException(s"Service missing. form data: $fastTrackInvitation"))
    invitationsService.checkVatRegistrationDateMatches(suppliedVrn, suppliedVatRegDate) flatMap {
      case Some(true) => createInvitation(arn, service, fastTrackInvitation.clientIdentifierType, Some(suppliedVrn), None)
      case Some(false) => fastTrackCache.save(FastTrackInvitation.newInstance.copy(service = fastTrackInvitation.service)).map { _ =>
        Redirect(routes.AgentsInvitationController.notMatched())
      }
      case None => fastTrackCache.save(FastTrackInvitation.newInstance.copy(service = fastTrackInvitation.service)).map { _ =>
        Redirect(routes.AgentsInvitationController.notEnrolled())
      }
    }
  }

  private def withMaybeContinueUrlCached[A](block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    withMaybeContinueUrl {
      case None => block
      case Some(url) => continueUrlStoreService.cacheContinueUrl(url).flatMap(_ => block)
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
        if (!fieldValue.matches("[0-9]{9}"))
          Invalid(ValidationError(regexFailure))
        else if (!Vrn.isValid(fieldValue.trim.toUpperCase))
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

  private def validateDate(value: String): Boolean = if (parseDate(value)) true else false

  private def parseDate(date: String): Boolean = {
    import org.joda.time.format.DateTimeFormat
    try {
      DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(date)
      true
    }
    catch {
      case _: Throwable => false
    }
  }

  private val invalidPostcode =
    validateField("error.postcode.required", "enter-postcode.invalid-format")(postcode => postcode.matches(postcodeRegex))

  import play.api.data.format.Formats.stringFormat

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)

  val agentInvitationNinoForm: Form[AgentInvitationUserInput] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText.verifying(invalidNino),
      "postcode" -> optional(text))
    ({ (service, clientIdentifier, _) => AgentInvitationUserInput(service, Some(Nino(clientIdentifier.trim.toUpperCase())), None) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), None)) }))
  }

  val agentInvitationVrnForm: Form[AgentInvitationVatForm] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText.verifying(invalidVrn),
      "registrationDate" -> optional(text))
    ({ (service, clientIdentifier, _) => AgentInvitationVatForm(service, Some(Vrn(clientIdentifier)), None) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.registrationDate)) }))
  }

  val agentInvitationVatRegistrationDateForm: Form[AgentInvitationVatForm] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText,
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
      "service" -> text,
      "clientIdentifier" -> normalizedText,
      "postcode" -> text.verifying(invalidPostcode))
    ({ (service, nino, postcode) => AgentInvitationUserInput(service, Some(Nino(nino.trim.toUpperCase())), Some(postcode)) })
    ({ user => Some((user.service, user.clientIdentifier.map(_.value).getOrElse(""), user.postcode.getOrElse(""))) }))
  }

  val agentFastTrackForm: Form[FastTrackInvitation] = {
    Form(mapping(
      "service" -> optional(text),
      "clientIdentifierType" -> optional(text),
      "clientIdentifier" -> optional(normalizedText),
      "postcode" -> optional(text),
      "vatRegDate" -> optional(text))
    ({ (service, clientIdType, clientId, postcode, vatRegDate) => FastTrackInvitation(service, clientIdType, clientId, postcode, vatRegDate) })
    ({ fastTrack => Some((fastTrack.service, fastTrack.clientIdentifierType, fastTrack.clientIdentifier, fastTrack.postcode, fastTrack.vatRegDate)) }))
  }

  object ClientForMtdItWithFlagOn {
    def unapply(arg: (AgentInvitationUserInput, FeatureFlags)): Option[TaxIdentifier] = arg match {
      case (AgentInvitationUserInput(HMRCMTDIT, Some(clientIdentifier), _), featureFlags) if featureFlags.showKfcMtdIt =>
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
    def unapply(arg: (AgentInvitationVatForm, FeatureFlags)): Option[TaxIdentifier] = arg match {
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

  object FastTrackInvitationItsaComplete {
    def unapply(arg: FastTrackInvitation): Option[FastTrackInvitation] = arg match {
      case FastTrackInvitation(Some(HMRCMTDIT), Some("ni"), Some(clientIdentifier), Some(postcode), _)
        if Nino.isValid(clientIdentifier) && postcode.matches(postcodeRegex) =>
        Some(arg.copy(vatRegDate = None))
      case _ => None
    }
  }

  object FastTrackInvitationIrvComplete {
    def unapply(arg: FastTrackInvitation): Option[FastTrackInvitation] = arg match {
      case FastTrackInvitation(Some(HMRCPIR), Some("ni"), Some(clientIdentifier), _, _)
        if Nino.isValid(clientIdentifier) =>
        Some(FastTrackInvitation(Some(HMRCPIR), Some("ni"), Some(clientIdentifier), None, None))
      case _ => None
    }
  }

  object FastTrackInvitationVatComplete {
    def unapply(arg: FastTrackInvitation): Option[FastTrackInvitation] = arg match {
      case FastTrackInvitation(Some(HMRCMTDVAT), Some("vrn"), Some(clientIdentifier), _, Some(vatRegDate))
        if Vrn.isValid(clientIdentifier) && validateDate(vatRegDate) =>
        Some(FastTrackInvitation(Some(HMRCMTDVAT), Some("vrn"), Some(clientIdentifier), None, Some(vatRegDate)))
      case _ => None
    }
  }

  object FastTrackInvitationNeedsClientIdentifier {
    def unapply(fastTrackInvitation: FastTrackInvitation): Option[FastTrackInvitation] = fastTrackInvitation match {
      case FastTrackInvitation(Some(service), _, Some(clientIdentifier), _, _) =>
        service match {
          case HMRCMTDVAT if !Vrn.isValid(clientIdentifier) =>
            Some(FastTrackInvitation(Some(HMRCMTDVAT), Some("vrn"), None, None, None))
          case HMRCMTDIT if !Nino.isValid(clientIdentifier) =>
            Some(FastTrackInvitation(Some(HMRCMTDIT), Some("ni"), None, None, None))
          case HMRCPIR if !Nino.isValid(clientIdentifier) =>
            Some(FastTrackInvitation(Some(HMRCPIR), Some("ni"), None, None, None))
          case _ => None
        }
      case FastTrackInvitation(Some(service), _, None, _, _) =>
        Some(FastTrackInvitation(Some(service), None, None, None, None))
      case _ => None
    }
  }

  object FastTrackInvitationNeedsKnownFact {
    def unapply(fastTrackInvitation: FastTrackInvitation): Option[FastTrackInvitation] = fastTrackInvitation match {
      case FastTrackInvitation(Some(HMRCMTDVAT), Some(_), Some(_), _, Some(vatRegDate)) if !validateDate(vatRegDate) =>
        Some(fastTrackInvitation.copy(vatRegDate = None))

      case FastTrackInvitation(Some(HMRCMTDIT), Some(_), Some(_), Some(postcode), _) if !postcode.matches(postcodeRegex) =>
        Some(fastTrackInvitation.copy(postcode = None))

      case FastTrackInvitation(Some(service), Some(_), Some(_), _, _) if service != Services.HMRCPIR =>
        Some(fastTrackInvitation.copy(postcode = None, vatRegDate = None))

      case _ => None
    }
  }

  object FastTrackInvitationNeedsService {
    def unapply(fastTrackInvitation: FastTrackInvitation): Option[FastTrackInvitation] = fastTrackInvitation match {
      case FastTrackInvitation(_, _, Some(_), _, _) =>
        Some(fastTrackInvitation.copy(service = None))

      case _ => None
    }
  }

}

