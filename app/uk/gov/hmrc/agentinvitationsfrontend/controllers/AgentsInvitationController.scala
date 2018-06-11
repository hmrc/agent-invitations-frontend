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
import play.api.data.format.Formats._
import play.api.data.validation._
import play.api.data.{Form, FormError, Mapping}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.{HeaderCarrier, Upstream4xxResponse}
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future
import scala.util.control.NonFatal

@Singleton
class AgentsInvitationController @Inject()(@Named("agent-invitations-frontend.external-url") externalUrl: String,
                                           @Named("agent-services-account-frontend.external-url") asAccUrl: String,
                                           invitationsService: InvitationsService,
                                           auditService: AuditService,
                                           fastTrackCache: FastTrackCache,
                                           continueUrlStoreService: ContinueUrlStoreService,
                                           val messagesApi: play.api.i18n.MessagesApi,
                                           val env: Environment,
                                           val authConnector: AuthConnector,
                                           val continueUrlActions: ContinueUrlActions,
                                           val withVerifiedPasscode: PasscodeVerification)
                                          (implicit val configuration: Configuration, val externalUrls: ExternalUrls, featureFlags: FeatureFlags)
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
  private[controllers] val agentServicesAccountUrl: String = if (isDevEnv) s"http://localhost:9401/agent-services-account" else "/agent-services-account"

  val agentInvitationIdentifyClientFormItsa: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationIdentifyClientFormItsa(featureFlags)

  val agentInvitationPostCodeForm: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

  val agentInvitationIdentifyClientFormVat: Form[UserInputVrnAndRegDate] =
    AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)

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
          val updateAggregate = fastTrackCache.fetch()
            .map(_.getOrElse(CurrentInvitationInput()))
            .map(_.copy(service = Some(userInput.service)))

          updateAggregate.flatMap(updateFastTrack =>
            fastTrackCache.save(updateFastTrack).flatMap(_ =>
              ifShouldShowService(updateFastTrack, featureFlags, isWhitelisted) {
                redirectBasedOnCurrentInputState(arn, updateFastTrack, isWhitelisted)
              }
            ))
        }
      )
    }
  }

  private val fastTrackToIdentifyClientFormItsa = (fastTrackDetails: CurrentInvitationInput) => {
    val service = fastTrackDetails.service.getOrElse("")
    val clientId = fastTrackDetails.clientIdentifier
    agentInvitationIdentifyClientFormItsa.fill(UserInputNinoAndPostcode(service, clientId, fastTrackDetails.postcode))
  }

  private val fastTrackToIdentifyClientFormVat = (fastTrackDetails: CurrentInvitationInput) => {
    val service = fastTrackDetails.service.getOrElse("")
    val clientId = fastTrackDetails.clientIdentifier
    agentInvitationIdentifyClientFormVat.fill(UserInputVrnAndRegDate(service, clientId, fastTrackDetails.vatRegDate))
  }

  val showIdentifyClientForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetch().map {
        case Some(inviteDetails) =>
          inviteDetails.service match {
            case Some(HMRCMTDIT) => Ok(identify_client_itsa(fastTrackToIdentifyClientFormItsa(inviteDetails)))
            case Some(HMRCMTDVAT) => Ok(identify_client_vat(fastTrackToIdentifyClientFormVat(inviteDetails)))
            case _ => Redirect(routes.AgentsInvitationController.selectService())
          }

        case None =>
          Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      serviceNameForm.bindFromRequest().fold(
        _ =>
          Future successful Redirect(routes.AgentsInvitationController.selectService()),
        {
          case HMRCMTDIT => identifyItsaClient(arn, isWhitelisted)
          case HMRCMTDVAT => identifyVatClient(arn, isWhitelisted)
          case _ => Future successful Redirect(routes.AgentsInvitationController.selectService())
        })
    }
  }

  def identifyItsaClient(arn: Arn, isWhitelisted: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    agentInvitationIdentifyClientFormItsa.bindFromRequest().fold(
      formWithErrors => {
        Future successful Ok(identify_client_itsa(formWithErrors))
      },
      userInput => for {
        maybeCachedInvitation <- fastTrackCache.fetch()
        invitationWithClientDetails = maybeCachedInvitation.getOrElse(CurrentInvitationInput()).copy(
          clientIdentifier = userInput.clientIdentifier,
          postcode = userInput.postcode
        )
        _ <- fastTrackCache.save(invitationWithClientDetails)
        redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
      } yield redirectResult
    )
  }

  def identifyVatClient(arn: Arn, isWhitelisted: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    agentInvitationIdentifyClientFormVat.bindFromRequest().fold(
      formWithErrors => {
        Future successful Ok(identify_client_vat(formWithErrors))
      },
      userInput => for {
        maybeCachedInvitation <- fastTrackCache.fetch()
        invitationWithClientDetails = maybeCachedInvitation.getOrElse(CurrentInvitationInput()).copy(
          clientIdentifier = userInput.clientIdentifier,
          vatRegDate = userInput.registrationDate
        )
        _ <- fastTrackCache.save(invitationWithClientDetails)
        redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
      } yield redirectResult
    )
  }

  val showNinoForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetch().flatMap {
        case Some(aggregate) =>
          val service = aggregate.service.getOrElse("")
          Future successful Ok(enter_nino(agentInvitationNinoForm.fill(UserInputNinoAndPostcode(service, None, aggregate.postcode))))
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
          val updatedAggregate = fastTrackCache.fetch()
            .map(_.getOrElse(CurrentInvitationInput()))
            .map(_.copy(clientIdentifier = userInput.clientIdentifier))

          updatedAggregate.flatMap(updatedInvitation =>
            fastTrackCache.save(updatedInvitation).flatMap { _ =>
              redirectBasedOnCurrentInputState(arn, updatedInvitation, isWhitelisted)
            })
        })
    }
  }

  val showPostcodeForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetch().flatMap {
        case Some(aggregate) =>
          val service = aggregate.service.getOrElse("")
          val nino = aggregate.clientIdentifier
          Future successful Ok(enter_postcode(agentInvitationPostCodeForm.fill(UserInputNinoAndPostcode(service, nino, None))))
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
          val updatedAggregate = fastTrackCache.fetch()
            .map(_.getOrElse(CurrentInvitationInput()))
            .map(_.copy(postcode = userInput.postcode))

          updatedAggregate.flatMap(updatedInvitation =>
            fastTrackCache.save(updatedInvitation).flatMap(_ =>
              redirectBasedOnCurrentInputState(arn, updatedInvitation, isWhitelisted)))
        })
    }
  }

  private[controllers] def createInvitation[T <: TaxIdentifier](arn: Arn, fti: FastTrackInvitation[T])(implicit request: Request[_]) = {
    invitationsService.createInvitation(arn, fti.service, fti.clientIdentifierType, fti.clientIdentifier, fti.knownFact)
      .map(invitation => {
        val id = invitation.selfUrl.toString.split("/").toStream.last
        if ((invitation.service == HMRCMTDIT && featureFlags.showKfcMtdIt)
          | (invitation.service == HMRCPIR && featureFlags.showKfcPersonalIncome)
          | (invitation.service == HMRCMTDVAT && featureFlags.showKfcMtdVat)) {
          auditService.sendAgentInvitationSubmitted(arn, id, fti, "Success")
        }
        else auditService.sendAgentInvitationSubmitted(arn, id, fti, "Not Required")
        Redirect(routes.AgentsInvitationController.invitationSent())
          .addingToSession(
            "invitationId" -> id,
            "deadline" -> invitation.expiryDate.toString(DateTimeFormat.forPattern("d MMMM YYYY"))
          )
      })
      .recoverWith {
        case noMtdItId: Upstream4xxResponse if noMtdItId.message.contains("CLIENT_REGISTRATION_NOT_FOUND") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Client Registration Not Found.")
          auditService.sendAgentInvitationSubmitted(arn, "", fti, "Fail", Some("CLIENT_REGISTRATION_NOT_FOUND"))
          Future successful Redirect(routes.AgentsInvitationController.notEnrolled())
        }
        case noPostCode: Upstream4xxResponse if noPostCode.message.contains("POSTCODE_DOES_NOT_MATCH") => {
          Logger.warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
          auditService.sendAgentInvitationSubmitted(arn, "", fti, "Fail", Some("POSTCODE_DOES_NOT_MATCH"))
          Future successful Redirect(routes.AgentsInvitationController.notMatched())
        }
        case e =>
          Logger.warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", fti, "Fail", Option(e.getMessage))
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
            _ <- fastTrackCache.save(CurrentInvitationInput())
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
      fastTrackCache.fetchAndClear().map {
        case Some(aggregate) => aggregate.service match {
          case Some(HMRCMTDVAT) =>
            Forbidden(not_enrolled(Services.messageKeyForVAT))
          case Some(HMRCMTDIT) =>
            Forbidden(not_enrolled(Services.messageKeyForITSA))
          case _ =>
            throw new Exception("Unsupported Service")
        }
        case None => throw new Exception("Empty Cache")
      }
    }
  }

  val notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetch().flatMap {
        case Some(aggregate) => aggregate.service match {
          case Some(HMRCMTDVAT) =>
            fastTrackCache.save(CurrentInvitationInput(HMRCMTDVAT)).map(_ =>
              Forbidden(not_matched(Services.messageKeyForVAT))
            )
          case Some(HMRCMTDIT) =>
            Future successful Forbidden(not_matched(Services.messageKeyForITSA))
          case _ =>
            throw new Exception("Unsupported Service")
        }
        case None => throw new Exception("Empty Cache")
      }
    }
  }

  val agentFastTrack: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      if (featureFlags.enableFastTrack) {
        agentFastTrackForm.bindFromRequest().fold(
          _ => Future successful Redirect(routes.AgentsInvitationController.selectService()),
          currentInvitationInput => {
            fastTrackCache.save(currentInvitationInput).flatMap { _ =>
              withMaybeContinueUrlCached {
                ifShouldShowService(currentInvitationInput, featureFlags, isWhitelisted) {
                  redirectBasedOnCurrentInputState(arn, currentInvitationInput, isWhitelisted)
                }
              }
            }
          })
      } else {
        Logger.warn("Fast-Track feature flag is switched off")
        Future successful BadRequest
      }
    }
  }

  def redirectBasedOnCurrentInputState(arn: Arn, currentInvitationInput: CurrentInvitationInput, isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] = {
    val updatedInvitationWithClientType = currentInvitationInput.copy(clientIdentifierType = currentInvitationInput.clientIdentifierTypeConversion)
    updatedInvitationWithClientType match {

      case CurrentInvitationInputVatReady(completeVatInvitation) =>
        completeVatInvitation.vatRegDate.map(LocalDate.parse) match {
          case Some(vatRegDate) =>
            invitationsService.checkVatRegistrationDateMatches(completeVatInvitation.clientIdentifier, vatRegDate) flatMap {
              case Some(true) => createInvitation(arn, completeVatInvitation)
              case Some(false) => fastTrackCache.save(currentInvitationInput).map { _ =>
                Redirect(routes.AgentsInvitationController.notMatched())
              }
              case None => fastTrackCache.save(currentInvitationInput).map { _ =>
                Redirect(routes.AgentsInvitationController.notEnrolled())
              }
            }
          case None =>
            createInvitation(arn, completeVatInvitation)
        }

      case CurrentInvitationInputItsaReady(completeItsaInvitation) =>
        createInvitation(arn, completeItsaInvitation)

      case CurrentInvitationInputPirReady(completePirInvitation) =>
        if (featureFlags.showKfcPersonalIncome)
          throw new Exception("KFC flagged as on, not implemented for personal-income-record")
        else
          createInvitation(arn, completePirInvitation)

      case CurrentInvitationInputNeedsService(_) =>
        Future successful Redirect(routes.AgentsInvitationController.selectService())

      case CurrentInvitationInputNeedsClientIdentifier(invitationNeedsClientIdentifier) => invitationNeedsClientIdentifier.service match {
        //Remove when implementing APB-16-11
        case Some(HMRCPIR) if isSupportedWhitelistedService(HMRCPIR, isWhitelisted) =>
          Future successful Redirect(routes.AgentsInvitationController.showNinoForm())
        case Some(service) if isSupportedWhitelistedService(service, isWhitelisted) =>
          Future successful Redirect(routes.AgentsInvitationController.showIdentifyClientForm())
        case _ =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
      }

      case CurrentInvitationInputNeedsKnownFact(invitationNeedsKnownFact) => invitationNeedsKnownFact.service match {
        case Some(service) if isSupportedWhitelistedService(service, isWhitelisted) =>
          Future successful Redirect(routes.AgentsInvitationController.showIdentifyClientForm())
        case _ =>
          Future successful Redirect(routes.AgentsInvitationController.selectService())
        }

      case _ =>
        Logger.warn("Resetting due to mix data in session")
        fastTrackCache.save(CurrentInvitationInput()).map(_ =>
          Redirect(routes.AgentsInvitationController.selectService()))
    }
  }

  private def ifShouldShowService(currentInvitationInput: CurrentInvitationInput, featureFlags: FeatureFlags, isWhitelisted: Boolean)
                                 (body: => Future[Result]): Future[Result] = {
    currentInvitationInput.service match {
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

  private def withMaybeContinueUrlCached[A](block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    withMaybeContinueUrl {
      case None => block
      case Some(url) => continueUrlStoreService.cacheContinueUrl(url).flatMap(_ => block)
    }
}

object AgentsInvitationController {

  import ValidateHelper._

  private val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  private val serviceChoice: Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError("error.service.required"))
  }

  private def validNino(nonEmptyFailure: String = "error.nino.required", invalidFailure: String = "enter-nino.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(nino => Nino.isValid(nino))

  private val validVrn =
    ValidateHelper.validateVrnField("error.vrn.required", "enter-vrn.regex-failure", "enter-vrn.checksum-failure")

  def validPostcode(isKfcFlagOn: Boolean, regexFailure: String, emptyFailure: String) = Constraint[String] { (input: String) =>
    if (isKfcFlagOn) {
      if (input.isEmpty) Invalid(ValidationError(emptyFailure))
      else if (!input.matches(postcodeRegex)) Invalid(ValidationError(regexFailure))
      else Valid
    } else Valid
  }

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)
  val trimmedUppercaseText: Mapping[String] = of[String].transform(_.trim.toUpperCase, identity)

  val serviceNameForm: Form[String] = Form(mapping("service" -> text)(identity)(Some(_)))

  def agentInvitationIdentifyClientFormItsa(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText.verifying(validNino(nonEmptyFailure = "identify-client.nino.required", invalidFailure = "identify-client.nino.invalid-format")),
      "postcode" -> optionalIf(featureFlags.showKfcMtdIt,
        trimmedUppercaseText.verifying(validPostcode(featureFlags.showKfcMtdIt, "identify-client.postcode.invalid-format", "identify-client.postcode.required"))
        )
    )
    ({ (service, clientIdentifier, postcode) => UserInputNinoAndPostcode(service, Some(clientIdentifier.trim.toUpperCase()), postcode) })
    ({ user => Some((user.service, user.clientIdentifier.getOrElse(""), user.postcode)) })
    )
  }

  def agentInvitationIdentifyClientFormVat(featureFlags: FeatureFlags): Form[UserInputVrnAndRegDate] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText.verifying(validVrn),
      "registrationDate" -> optionalIf(featureFlags.showKfcMtdVat, DateFieldHelper.dateFieldsMapping))
    ({ (service, clientIdentifier, registrationDate) => UserInputVrnAndRegDate(service, Some(clientIdentifier.trim.toUpperCase()), registrationDate) })
    ({ user => Some((user.service, user.clientIdentifier.getOrElse(""), user.registrationDate)) })
    )
  }

  val agentInvitationNinoForm: Form[UserInputNinoAndPostcode] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText.verifying(validNino()),
      "postcode" -> optional(trimmedUppercaseText))
    ({ (service, clientIdentifier, _) => UserInputNinoAndPostcode(service, Some(clientIdentifier.trim.toUpperCase()), None) })
    ({ user => Some((user.service, user.clientIdentifier.getOrElse(""), None)) }))
  }

  val agentInvitationServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(mapping(
      "service" -> text.verifying(serviceChoice),
      "clientIdentifier" -> optional(normalizedText),
      "postcode" -> optional(text))
    ({ (service, _, _) => UserInputNinoAndPostcode(service, None, None) })
    ({ user => Some((user.service, None, None)) }))
  }

  def agentInvitationPostCodeForm(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] = {
    Form(mapping(
      "service" -> text,
      "clientIdentifier" -> normalizedText,
      "postcode" -> optionalIf(featureFlags.showKfcMtdIt,
        trimmedUppercaseText.verifying(validPostcode(featureFlags.showKfcMtdIt, "enter-postcode.invalid-format", "error.postcode.required")))
    )
    ({ (service, nino, postcode) => UserInputNinoAndPostcode(service, Some(nino.trim.toUpperCase()), postcode) })
    ({ user => Some((user.service, user.clientIdentifier.getOrElse(""), user.postcode)) }))
  }

  val agentFastTrackForm: Form[CurrentInvitationInput] = {
    Form(mapping(
      "service" -> optional(text),
      "clientIdentifierType" -> optional(text),
      "clientIdentifier" -> optional(normalizedText),
      "postcode" -> optional(trimmedUppercaseText),
      "vatRegDate" -> optional(text))
    ({ (service, clientIdType, clientId, postcode, vatRegDate) => CurrentInvitationInput(service, clientIdType, clientId, postcode, vatRegDate) })
    ({ fastTrack => Some((fastTrack.service, fastTrack.clientIdentifierType, fastTrack.clientIdentifier, fastTrack.postcode, fastTrack.vatRegDate)) }))
  }

  object ClientForMtdItWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[String] = arg match {
      case (UserInputNinoAndPostcode(HMRCMTDIT, Some(clientIdentifier), _), featureFlags) if featureFlags.showKfcMtdIt =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientForPirWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputNinoAndPostcode(HMRCPIR, Some(_), _), featureFlags) if featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientWithItsaOrPirFlagOff {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputNinoAndPostcode(_, Some(_), _), featureFlags) if !featureFlags.showKfcMtdIt || !featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientForVatWithFlagOn {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[String] = arg match {
      case (UserInputVrnAndRegDate(HMRCMTDVAT, Some(clientIdentifier), _), featureFlags) if featureFlags.showKfcMtdVat =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientWithVatFlagOff {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputVrnAndRegDate(_, Some(_), _), featureFlags) if !featureFlags.showKfcMtdVat =>
        Some(())
      case _ => None
    }
  }

  object CurrentInvitationInputItsaReady {
    def unapply(arg: CurrentInvitationInput)(implicit featureFlags: FeatureFlags): Option[FastTrackItsaInvitation] = arg match {
      case CurrentInvitationInput(Some(HMRCMTDIT), Some("ni"), Some(clientIdentifier), postcodeOpt, _)
        if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcMtdIt || postcodeOpt.exists(_.matches(postcodeRegex))) =>
          Some(FastTrackItsaInvitation(Nino(clientIdentifier),
            if(featureFlags.showKfcMtdIt) postcodeOpt else None))
      case _ => None
    }
  }

  object CurrentInvitationInputPirReady {
    def unapply(arg: CurrentInvitationInput): Option[FastTrackPirInvitation] = arg match {
      case CurrentInvitationInput(Some(HMRCPIR), Some("ni"), Some(clientIdentifier), _, _)
        if Nino.isValid(clientIdentifier) =>
        Some(FastTrackPirInvitation(Nino(clientIdentifier)))
      case _ => None
    }
  }

  object CurrentInvitationInputVatReady {
    def unapply(arg: CurrentInvitationInput)(implicit featureFlags: FeatureFlags): Option[FastTrackVatInvitation] = arg match {
      case CurrentInvitationInput(Some(HMRCMTDVAT), Some("vrn"), Some(clientIdentifier), _, vatRegDateOpt)
        if Vrn.isValid(clientIdentifier) && (!featureFlags.showKfcMtdVat || vatRegDateOpt.exists(DateFieldHelper.validateDate)) =>
          Some(FastTrackVatInvitation(Vrn(clientIdentifier),
              if(featureFlags.showKfcMtdVat) vatRegDateOpt else None)
          )
      case _ => None
    }
  }

  object CurrentInvitationInputNeedsClientIdentifier {
    def unapply(currentInvitationInput: CurrentInvitationInput): Option[CurrentInvitationInput] = currentInvitationInput match {
      case CurrentInvitationInput(Some(service), _, Some(clientIdentifier), _, _) =>
        service match {
          case HMRCMTDVAT if !Vrn.isValid(clientIdentifier) =>
            Some(CurrentInvitationInput(Some(HMRCMTDVAT), Some("vrn"), None, None, None))
          case HMRCMTDIT if !Nino.isValid(clientIdentifier) =>
            Some(CurrentInvitationInput(Some(HMRCMTDIT), Some("ni"), None, None, None))
          case HMRCPIR if !Nino.isValid(clientIdentifier) =>
            Some(CurrentInvitationInput(Some(HMRCPIR), Some("ni"), None, None, None))
          case _ => None
        }
      case CurrentInvitationInput(Some(service), _, None, _, _) =>
        Some(CurrentInvitationInput(service))
      case _ => None
    }
  }

  object CurrentInvitationInputNeedsKnownFact {
    def unapply(currentInvitationInput: CurrentInvitationInput): Option[CurrentInvitationInput] = currentInvitationInput match {
      case CurrentInvitationInput(Some(HMRCMTDVAT), Some(_), Some(_), _, Some(vatRegDate)) if !DateFieldHelper.validateDate(vatRegDate) =>
        Some(currentInvitationInput.copy(vatRegDate = None))

      case CurrentInvitationInput(Some(HMRCMTDIT), Some(_), Some(_), Some(postcode), _) if !postcode.matches(postcodeRegex) =>
        Some(currentInvitationInput.copy(postcode = None))

      case CurrentInvitationInput(Some(service), Some(_), Some(_), _, _) if service != Services.HMRCPIR =>
        Some(currentInvitationInput.copy(postcode = None, vatRegDate = None))

      case _ => None
    }
  }

  object CurrentInvitationInputNeedsService {
    def unapply(currentInvitationInput: CurrentInvitationInput): Option[CurrentInvitationInput] =
      currentInvitationInput.service match {
        case Some(_) =>
          None
        case None =>
          Some(currentInvitationInput)
      }

  }

}

object DateFieldHelper {

  def validateDate(value: String): Boolean = if (parseDate(value)) true else false

  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd")

  def parseDate(date: String): Boolean = {
    try {
      dateTimeFormat.parseDateTime(date)
      true
    }
    catch {
      case _: Throwable => false
    }
  }

  def parseDateIntoFields(date: String): Option[(String, String, String)] = {
    try {
      val l = dateTimeFormat.parseLocalDate(date)
      Some((l.getYear.toString, l.getMonthOfYear.toString, l.getDayOfMonth.toString))
    }
    catch {
      case NonFatal(_) => None
    }
  }

  val formatDateFromFields: (String, String, String) => String = {
    case (y, m, d) =>
      if(y.isEmpty || m.isEmpty || d.isEmpty) ""
      else {
        val month = if (m.length == 1) "0" + m else m
        val day = if (d.length == 1) "0" + d else d
        s"$y-$month-$day"
      }
  }

  val validVatDateFormat: Constraint[String] = ValidateHelper.validateField("error.vat-registration-date.required", "enter-vat-registration-date.invalid-format")(vatRegistrationDate => validateDate(vatRegistrationDate))

  val dateFieldsMapping: Mapping[String] = mapping("year" -> text, "month" -> text, "day" -> text)(formatDateFromFields)(parseDateIntoFields).verifying(validVatDateFormat)

}

object ValidateHelper {

  def nonEmpty(failure: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.isEmpty) Invalid(ValidationError(failure)) else Valid
  }

  def validateField(emptyFailure: String, invalidFailure: String)(condition: String => Boolean) = Constraint[String] { fieldValue: String =>
    nonEmpty(emptyFailure)(fieldValue) match {
      case i: Invalid =>
        i
      case Valid =>
        if (condition(fieldValue.trim.toUpperCase))
          Valid
        else
          Invalid(ValidationError(invalidFailure))
    }
  }

  def validateVrnField(nonEmptyFailure: String,
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

  def optionalIf[A](isOn: Boolean, mapping: Mapping[A]): Mapping[Option[A]] = OptionalMappingIf(isOn, mapping)

}

case class OptionalMappingIf[T](isOn: Boolean, wrapped: Mapping[T], val constraints: Seq[Constraint[Option[T]]] = Nil) extends Mapping[Option[T]] {

  override val format: Option[(String, Seq[Any])] = wrapped.format

  val key = wrapped.key

  def verifying(addConstraints: Constraint[Option[T]]*): Mapping[Option[T]] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  def bind(data: Map[String, String]): Either[Seq[FormError], Option[T]] = if(isOn) {
    data.keys.filter(p => p == key || p.startsWith(key + ".") || p.startsWith(key + "["))
      .map(k => data
        .get(k)
        .filterNot(_.isEmpty))
      .collectFirst { case Some(v) => v }
      .map { _ =>
        wrapped.bind(data).right.map(Some(_))
      }.getOrElse {
        wrapped.bind(data ++ Map(key -> "")).right.map(Some(_))
    }.right.flatMap(applyConstraints)
  } else Right(None)

  def unbind(value: Option[T]): Map[String, String] = {
    value.map(wrapped.unbind).getOrElse(Map.empty)
  }

  def unbindAndValidate(value: Option[T]): (Map[String, String], Seq[FormError]) = {
    val errors = collectErrors(value)
    value.map(wrapped.unbindAndValidate).map(r => r._1 -> (r._2 ++ errors)).getOrElse(Map.empty -> errors)
  }

  def withPrefix(prefix: String): Mapping[Option[T]] = {
    copy(wrapped = wrapped.withPrefix(prefix))
  }

  val mappings: Seq[Mapping[_]] = wrapped.mappings

}

