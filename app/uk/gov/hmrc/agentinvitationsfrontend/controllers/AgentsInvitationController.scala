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
import play.api.data.{Form, Mapping}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, _}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, InvitationId, Vrn}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.binders.ContinueUrl
import uk.gov.hmrc.play.bootstrap.controller.{ActionWithMdc, FrontendController}

import scala.concurrent.Future

@Singleton
class AgentsInvitationController @Inject()(
  @Named("agent-invitations-frontend.external-url") externalUrl: String,
  @Named("agent-services-account-frontend.external-url") asAccUrl: String,
  invitationsService: InvitationsService,
  auditService: AuditService,
  fastTrackCache: FastTrackCache,
  continueUrlStoreService: ContinueUrlStoreService,
  val messagesApi: play.api.i18n.MessagesApi,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags)
    extends FrontendController with I18nSupport with AuthActions {

  import AgentsInvitationController._
  import continueUrlActions._

  private val personalIncomeRecord =
    if (featureFlags.showPersonalIncome)
      Seq(HMRCPIR -> Messages("select-service.personal-income-viewer"))
    else Seq.empty
  private val mtdItId = if (featureFlags.showHmrcMtdIt) Seq(HMRCMTDIT -> Messages("select-service.itsa")) else Seq.empty
  private val vat = if (featureFlags.showHmrcMtdVat) Seq(HMRCMTDVAT   -> Messages("select-service.vat")) else Seq.empty

  private def enabledServices(isWhitelisted: Boolean): Seq[(String, String)] =
    if (isWhitelisted) {
      personalIncomeRecord ++ mtdItId ++ vat
    } else {
      mtdItId ++ vat
    }

  private[controllers] val isDevEnv =
    if (env.mode.equals(Mode.Test)) false else configuration.getString("run.mode").forall(Mode.Dev.toString.equals)
  private[controllers] val agentServicesAccountUrl: String =
    if (isDevEnv) s"http://localhost:9401/agent-services-account" else "/agent-services-account"

  val agentInvitationIdentifyClientFormItsa: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationIdentifyClientFormItsa(featureFlags)

  val agentInvitationPostCodeForm: Form[UserInputNinoAndPostcode] =
    AgentsInvitationController.agentInvitationPostCodeForm(featureFlags)

  val agentInvitationIdentifyClientFormVat: Form[UserInputVrnAndRegDate] =
    AgentsInvitationController.agentInvitationIdentifyClientFormVat(featureFlags)

  val agentInvitationIdentifyClientFormIrv: Form[UserInputNinoAndDob] =
    AgentsInvitationController.agentInvitationIdentifyClientFormIrv(featureFlags)

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
      agentInvitationServiceForm
        .bindFromRequest()
        .fold(
          formWithErrors => {
            Future successful Ok(select_service(formWithErrors, allowedServices))
          },
          userInput => {
            val updateAggregate = fastTrackCache
              .fetch()
              .map(_.getOrElse(CurrentInvitationInput()))
              .map(_.copy(service = Some(userInput.service)))

            updateAggregate.flatMap(
              updateFastTrack =>
                fastTrackCache
                  .save(updateFastTrack)
                  .flatMap(_ =>
                    ifShouldShowService(updateFastTrack, featureFlags, isWhitelisted) {
                      redirectBasedOnCurrentInputState(arn, updateFastTrack, isWhitelisted)
                  }))
          }
        )
    }
  }

  private val fastTrackToIdentifyClientFormItsa = (fastTrackDetails: CurrentInvitationInput) => {
    val service = fastTrackDetails.service.getOrElse("")
    val clientId = fastTrackDetails.clientIdentifier
    agentInvitationIdentifyClientFormItsa.fill(UserInputNinoAndPostcode(service, clientId, fastTrackDetails.knownFact))
  }

  private val fastTrackToIdentifyClientFormVat = (fastTrackDetails: CurrentInvitationInput) => {
    val service = fastTrackDetails.service.getOrElse("")
    val clientId = fastTrackDetails.clientIdentifier
    agentInvitationIdentifyClientFormVat.fill(UserInputVrnAndRegDate(service, clientId, fastTrackDetails.knownFact))
  }

  private val fastTrackToIdentifyClientFormIrv = (fastTrackDetails: CurrentInvitationInput) => {
    val service = fastTrackDetails.service.getOrElse("")
    val clientId = fastTrackDetails.clientIdentifier
    agentInvitationIdentifyClientFormIrv.fill(UserInputNinoAndDob(service, clientId, fastTrackDetails.knownFact))
  }

  val showIdentifyClientForm: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetch().map {
        case Some(inviteDetails) =>
          inviteDetails.service match {
            case Some(HMRCMTDIT) =>
              Ok(identify_client_itsa(fastTrackToIdentifyClientFormItsa(inviteDetails), featureFlags.showKfcMtdIt))
            case Some(HMRCMTDVAT) =>
              Ok(identify_client_vat(fastTrackToIdentifyClientFormVat(inviteDetails), featureFlags.showKfcMtdVat))
            case Some(HMRCPIR) =>
              Ok(
                identify_client_irv(
                  fastTrackToIdentifyClientFormIrv(inviteDetails),
                  featureFlags.showKfcPersonalIncome))
            case _ => Redirect(routes.AgentsInvitationController.selectService())
          }

        case None =>
          Redirect(routes.AgentsInvitationController.selectService())
      }
    }
  }

  val submitIdentifyClient: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (arn, isWhitelisted) =>
      serviceNameForm
        .bindFromRequest()
        .fold(
          _ => Future successful Redirect(routes.AgentsInvitationController.selectService()), {
            case HMRCMTDIT  => identifyItsaClient(arn, isWhitelisted)
            case HMRCMTDVAT => identifyVatClient(arn, isWhitelisted)
            case HMRCPIR    => identifyIrvClient(arn, isWhitelisted)
            case _          => Future successful Redirect(routes.AgentsInvitationController.selectService())
          }
        )
    }
  }

  def identifyItsaClient(arn: Arn, isWhitelisted: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier) =
    agentInvitationIdentifyClientFormItsa
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(identify_client_itsa(formWithErrors, featureFlags.showKfcMtdIt))
        },
        userInput =>
          for {
            maybeCachedInvitation <- fastTrackCache.fetch()
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentInvitationInput())
              .copy(
                clientIdentifier = userInput.clientIdentifier,
                clientIdentifierType = Some("ni"),
                knownFact = userInput.postcode
              )
            _              <- fastTrackCache.save(invitationWithClientDetails)
            redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
          } yield redirectResult
      )

  def identifyVatClient(arn: Arn, isWhitelisted: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier) =
    agentInvitationIdentifyClientFormVat
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(identify_client_vat(formWithErrors, featureFlags.showKfcMtdVat))
        },
        userInput =>
          for {
            maybeCachedInvitation <- fastTrackCache.fetch()
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentInvitationInput())
              .copy(
                clientIdentifier = userInput.clientIdentifier,
                clientIdentifierType = Some("vrn"),
                knownFact = userInput.registrationDate
              )
            _              <- fastTrackCache.save(invitationWithClientDetails)
            redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
          } yield redirectResult
      )

  def identifyIrvClient(arn: Arn, isWhitelisted: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier) =
    agentInvitationIdentifyClientFormIrv
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future successful Ok(identify_client_irv(formWithErrors, featureFlags.showKfcPersonalIncome))
        },
        userInput =>
          for {
            maybeCachedInvitation <- fastTrackCache.fetch()
            invitationWithClientDetails = maybeCachedInvitation
              .getOrElse(CurrentInvitationInput())
              .copy(
                clientIdentifier = userInput.clientIdentifier,
                clientIdentifierType = Some("ni"),
                knownFact = userInput.dob
              )
            _              <- fastTrackCache.save(invitationWithClientDetails)
            redirectResult <- redirectBasedOnCurrentInputState(arn, invitationWithClientDetails, isWhitelisted)
          } yield redirectResult
      )

  private[controllers] def createInvitation[T <: TaxIdentifier](arn: Arn, fti: FastTrackInvitation[T])(
    implicit request: Request[_]) =
    invitationsService
      .createInvitation(arn, fti.service, fti.clientIdentifierType, fti.clientIdentifier)
      .map(invitation => {
        val id = invitation.selfUrl.toString.split("/").toStream.last
        if ((invitation.service == HMRCMTDIT && featureFlags.showKfcMtdIt)
              | (invitation.service == HMRCPIR && featureFlags.showKfcPersonalIncome)
              | (invitation.service == HMRCMTDVAT && featureFlags.showKfcMtdVat)) {
          auditService.sendAgentInvitationSubmitted(arn, id, fti, "Success")
        } else auditService.sendAgentInvitationSubmitted(arn, id, fti, "Not Required")
        Redirect(routes.AgentsInvitationController.invitationSent())
          .addingToSession(
            "invitationId" -> id,
            "deadline"     -> invitation.expiryDate.toString(DateTimeFormat.forPattern("d MMMM YYYY"))
          )
      })
      .recoverWith {
        case e =>
          Logger(getClass).warn(s"Invitation Creation Failed: ${e.getMessage}")
          auditService.sendAgentInvitationSubmitted(arn, "", fti, "Fail", Option(e.getMessage))
          Future.failed(e)
      }

  val invitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      Logger(getClass).info(
        s"Session contains ${request.session.get("invitationId")} ${request.session.get("deadline")}")
      (request.session.get("invitationId"), request.session.get("deadline")) match {
        case (Some(id), Some(deadline)) =>
          val invitationUrl: String =
            s"$externalUrl${routes.ClientsInvitationController.start(InvitationId(id)).path()}"
          for {
            _        <- fastTrackCache.save(CurrentInvitationInput())
            continue <- continueUrlStoreService.fetchContinueUrl
          } yield Ok(invitation_sent(invitationUrl, deadline, continue.isDefined, featureFlags.enableTrackRequests))
        case _ =>
          throw new RuntimeException("User attempted to browse to invitationSent")
      }
    }
  }

  val continueAfterInvitationSent: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      for {
        continue <- continueUrlStoreService.fetchContinueUrl.map(continue =>
                     continue.getOrElse(ContinueUrl(agentServicesAccountUrl)))
        _ <- continueUrlStoreService.remove()
      } yield Redirect(continue.url)
    }
  }

  val notEnrolled: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      fastTrackCache.fetch().map {
        case Some(aggregate) =>
          aggregate.service match {
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
        case Some(aggregate) =>
          aggregate.service match {
            case Some(HMRCMTDVAT) =>
              Future successful Forbidden(not_matched(Services.messageKeyForVAT))
            case Some(HMRCMTDIT) =>
              Future successful Forbidden(not_matched(Services.messageKeyForITSA))
            case Some(HMRCPIR) =>
              Future successful Forbidden(not_matched(Services.messageKeyForAfi))
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
        agentFastTrackForm
          .bindFromRequest()
          .fold(
            _ => Future successful Redirect(routes.AgentsInvitationController.selectService()),
            currentInvitationInput => {
              val fastTrackInput = currentInvitationInput.copy(fromFastTrack = true)
              fastTrackCache.save(fastTrackInput).flatMap { _ =>
                withMaybeContinueUrlCached {
                  ifShouldShowService(fastTrackInput, featureFlags, isWhitelisted) {
                    redirectBasedOnCurrentInputState(arn, fastTrackInput, isWhitelisted)
                  }
                }
              }
            }
          )
      } else {
        Logger(getClass).warn("Fast-Track feature flag is switched off")
        Future successful BadRequest
      }
    }
  }

  private[controllers] def knownFactCheckVat(
    arn: Arn,
    currentInvitationInput: CurrentInvitationInput,
    fastTrackVatInvitation: FastTrackVatInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    fastTrackVatInvitation.vatRegDate.map(date => LocalDate.parse(date.value)) match {
      case Some(vatRegDate) =>
        invitationsService
          .checkVatRegistrationDateMatches(fastTrackVatInvitation.clientIdentifier, vatRegDate) flatMap {
          case Some(true) =>
            createInvitation(arn, fastTrackVatInvitation)
          case Some(false) =>
            fastTrackCache.save(currentInvitationInput).map { _ =>
              Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Date Does Not Match.")
              Redirect(routes.AgentsInvitationController.notMatched())
            }
          case None =>
            fastTrackCache.save(currentInvitationInput).map { _ =>
              Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: VAT Registration Not Found.")
              Redirect(routes.AgentsInvitationController.notEnrolled())
            }
        }
      case None =>
        createInvitation(arn, fastTrackVatInvitation)
    }

  private[controllers] def knownFactCheckItsa(
    arn: Arn,
    currentInvitationInput: CurrentInvitationInput,
    fastTrackItsaInvitation: FastTrackItsaInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    fastTrackItsaInvitation.postcode match {
      case Some(postcode) =>
        for {
          hasPostcode <- invitationsService
                          .checkPostcodeMatches(fastTrackItsaInvitation.clientIdentifier, postcode.value)
          result <- hasPostcode match {
                     case Some(true) =>
                       createInvitation(arn, fastTrackItsaInvitation)
                     case Some(false) =>
                       fastTrackCache.save(currentInvitationInput).map { _ =>
                         Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Postcode Does Not Match.")
                         auditService.sendAgentInvitationSubmitted(
                           arn,
                           "",
                           fastTrackItsaInvitation,
                           "Fail",
                           Some("POSTCODE_DOES_NOT_MATCH"))
                         Redirect(routes.AgentsInvitationController.notMatched())
                       }
                     case None =>
                       fastTrackCache.save(currentInvitationInput).map { _ =>
                         Logger(getClass).warn(
                           s"${arn.value}'s Invitation Creation Failed: Client Registration Not Found.")
                         auditService.sendAgentInvitationSubmitted(
                           arn,
                           "",
                           fastTrackItsaInvitation,
                           "Fail",
                           Some("CLIENT_REGISTRATION_NOT_FOUND"))
                         Redirect(routes.AgentsInvitationController.notEnrolled())
                       }
                   }
        } yield result
      case None =>
        createInvitation(arn, fastTrackItsaInvitation)
    }

  private[controllers] def knownFactCheckIrv(
    arn: Arn,
    currentInvitationInput: CurrentInvitationInput,
    fastTrackPirInvitation: FastTrackPirInvitation,
    isWhitelisted: Boolean)(implicit request: Request[_]) =
    if (featureFlags.showKfcPersonalIncome) {
      Logger(getClass).warn("KFC flagged as on, not implemented for personal-income-record")
      fastTrackPirInvitation.dob match {
        case Some(dob) =>
          invitationsService
            .checkCitizenRecordMatches(fastTrackPirInvitation.clientIdentifier, LocalDate.parse(dob.value))
            .flatMap {
              case Some(true) =>
                createInvitation(arn, fastTrackPirInvitation)
              case Some(false) =>
                Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: Not Matched from Citizen-Details.")
                Future successful Redirect(routes.AgentsInvitationController.notMatched())
              case None =>
                Logger(getClass).warn(
                  s"${arn.value}'s Invitation Creation Failed: No Record found from Citizen-Details.")
                Future successful Redirect(routes.AgentsInvitationController.notMatched())
            }
        case None =>
          Logger(getClass).warn(s"${arn.value}'s Invitation Creation Failed: No KnownFact Provided")
          Future successful Redirect(routes.AgentsInvitationController.notMatched())
      }
    } else {
      createInvitation(arn, fastTrackPirInvitation)
    }

  private[controllers] def confirmAndRedirect(
    arn: Arn,
    currentInvitationInput: CurrentInvitationInput,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    currentInvitationInput match {
      case CurrentInvitationInputItsaReady(completeItsaInvitation) =>
        createInvitation(arn, completeItsaInvitation)
      case CurrentInvitationInputVatReady(completeVatInvitation) =>
        createInvitation(arn, completeVatInvitation)
      case CurrentInvitationInputPirReady(completeIrvInvitation) =>
        createInvitation(arn, completeIrvInvitation)
      case _ =>
        Logger(getClass).warn(s"Something has gone wrong. Redirected back to check current state.")
        redirectBasedOnCurrentInputState(arn, currentInvitationInput, isWhitelisted)
    }

  def redirectBasedOnCurrentInputState(
    arn: Arn,
    currentInvitationInput: CurrentInvitationInput,
    isWhitelisted: Boolean)(implicit request: Request[_]): Future[Result] =
    currentInvitationInput match {
      case CurrentInvitationInputVatReady(completeVatInvitation) =>
        knownFactCheckVat(arn, currentInvitationInput, completeVatInvitation, isWhitelisted)

      case CurrentInvitationInputItsaReady(completeItsaInvitation) =>
        knownFactCheckItsa(arn, currentInvitationInput, completeItsaInvitation, isWhitelisted)

      case CurrentInvitationInputPirReady(completePirInvitation) =>
        knownFactCheckIrv(arn, currentInvitationInput, completePirInvitation, isWhitelisted)

      case CurrentInvitationInputNeedsService(_) =>
        Future successful Redirect(routes.AgentsInvitationController.selectService())

      case CurrentInvitationInputNeedsClientIdentifier(invitationNeedsClientIdentifier) =>
        invitationNeedsClientIdentifier.service match {
          case Some(service) if isSupportedWhitelistedService(service, isWhitelisted) =>
            Future successful Redirect(routes.AgentsInvitationController.showIdentifyClientForm())
          case _ =>
            Future successful Redirect(routes.AgentsInvitationController.selectService())
        }

      case CurrentInvitationInputNeedsKnownFact(invitationNeedsKnownFact) =>
        invitationNeedsKnownFact.service match {
          case Some(service) if isSupportedWhitelistedService(service, isWhitelisted) =>
            Future successful Redirect(routes.AgentsInvitationController.showIdentifyClientForm())
          case _ =>
            Future successful Redirect(routes.AgentsInvitationController.selectService())
        }

      case _ =>
        Logger(getClass).warn("Resetting due to mix data in session")
        fastTrackCache
          .save(CurrentInvitationInput())
          .map(_ => Redirect(routes.AgentsInvitationController.selectService()))
    }

  private def ifShouldShowService(
    currentInvitationInput: CurrentInvitationInput,
    featureFlags: FeatureFlags,
    isWhitelisted: Boolean)(body: => Future[Result]): Future[Result] =
    currentInvitationInput.service match {
      case Some(HMRCPIR) if !isWhitelisted =>
        Logger(getClass).warn(s"User is not whitelisted to create $HMRCPIR invitation")
        Future successful BadRequest
      case Some(HMRCMTDVAT) if !featureFlags.showHmrcMtdVat =>
        Logger(getClass).warn(s"Service: $HMRCMTDVAT feature flagged is switched off")
        Future successful BadRequest
      case Some(HMRCMTDIT) if !featureFlags.showHmrcMtdIt =>
        Logger(getClass).warn(s"Service: $HMRCMTDIT feature flagged is switched off")
        Future successful BadRequest
      case Some(HMRCPIR) if !featureFlags.showPersonalIncome =>
        Logger(getClass).warn(s"Service: $HMRCPIR feature flagged is switched off")
        Future successful BadRequest
      case _ => body
    }

  private def isSupportedWhitelistedService(service: String, isWhitelisted: Boolean): Boolean =
    enabledServices(isWhitelisted).exists(_._1 == service)

  private def withMaybeContinueUrlCached[A](
    block: => Future[Result])(implicit hc: HeaderCarrier, request: Request[A]): Future[Result] =
    withMaybeContinueUrl {
      case None      => block
      case Some(url) => continueUrlStoreService.cacheContinueUrl(url).flatMap(_ => block)
    }
}

object AgentsInvitationController {

  import ValidateHelper._
  import DateFieldHelper._

  private val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  private val postcodeCharactersRegex = "^[a-zA-Z0-9 ]+$"

  private val serviceChoice: Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError("error.service.required"))
  }

  private val confirmationChoice: Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError("error.confirm-client.required"))
  }

  private def validNino(
    nonEmptyFailure: String = "error.nino.required",
    invalidFailure: String = "enter-nino.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(nino => Nino.isValid(nino))

  private val validVrn =
    ValidateHelper.validateVrnField("error.vrn.required", "enter-vrn.regex-failure", "enter-vrn.checksum-failure")

  def validPostcode(
    isKfcFlagOn: Boolean,
    invalidFormatFailure: String,
    emptyFailure: String,
    invalidCharactersFailure: String) = Constraint[String] { input: String =>
    if (isKfcFlagOn) {
      if (input.isEmpty) Invalid(ValidationError(emptyFailure))
      else if (!input.matches(postcodeCharactersRegex)) Invalid(ValidationError(invalidCharactersFailure))
      else if (!input.matches(postcodeRegex)) Invalid(ValidationError(invalidFormatFailure))
      else Valid
    } else Valid
  }

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)
  val trimmedUppercaseText: Mapping[String] = of[String].transform(_.trim.toUpperCase, identity)

  val serviceNameForm: Form[String] = Form(mapping("service" -> text)(identity)(Some(_)))

  def agentInvitationIdentifyClientFormItsa(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "service" -> text,
        "clientIdentifier" -> normalizedText.verifying(
          validNino(nonEmptyFailure = "error.nino.required", invalidFailure = "enter-nino.invalid-format")),
        "knownFact" -> optionalIf(
          featureFlags.showKfcMtdIt,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showKfcMtdIt,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (service, clientIdentifier, postcode) =>
        UserInputNinoAndPostcode(service, Some(clientIdentifier.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

  def agentInvitationIdentifyClientFormVat(featureFlags: FeatureFlags): Form[UserInputVrnAndRegDate] =
    Form(
      mapping(
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validVrn),
        "knownFact"        -> optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))
      )({ (service, clientIdentifier, registrationDate) =>
        UserInputVrnAndRegDate(service, Some(clientIdentifier.trim.toUpperCase()), registrationDate)
      })({ user =>
        Some((user.service, user.clientIdentifier.getOrElse(""), user.registrationDate))
      }))

  def agentInvitationIdentifyClientFormIrv(featureFlags: FeatureFlags): Form[UserInputNinoAndDob] =
    Form(
      mapping(
        "service" -> text,
        "clientIdentifier" -> normalizedText.verifying(
          validNino(nonEmptyFailure = "error.nino.required", invalidFailure = "enter-nino.invalid-format")),
        "knownFact" -> optionalIf(
          featureFlags.showKfcPersonalIncome,
          dateFieldsMapping(validDobDateFormat)
        )
      )({ (service, clientIdentifier, dob) =>
        UserInputNinoAndDob(service, Some(clientIdentifier.trim.toUpperCase()), dob)
      })({ user =>
        Some((user.service, user.clientIdentifier.getOrElse(""), user.dob))
      }))

  val agentInvitationServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "service"          -> text.verifying(serviceChoice),
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text))({ (service, _, _) =>
        UserInputNinoAndPostcode(service, None, None)
      })({ user =>
        Some((user.service, None, None))
      }))
  }

  val agentConfirmClientForm: Form[Confirmation] = {
    Form(
      mapping(
        "choice" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice)
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))
  }

  def agentInvitationPostCodeForm(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "service"          -> text,
        "clientIdentifier" -> normalizedText,
        "knownFact" -> optionalIf(
          featureFlags.showKfcMtdIt,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showKfcMtdIt,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (service, nino, postcode) =>
        UserInputNinoAndPostcode(service, Some(nino.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

  val agentFastTrackForm: Form[CurrentInvitationInput] = {
    Form(
      mapping(
        "service"              -> optional(text),
        "clientIdentifierType" -> optional(text),
        "clientIdentifier"     -> optional(normalizedText),
        "knownFact"            -> optional(text)
      )({ (service, clientIdType, clientId, knownFact) =>
        CurrentInvitationInput(service, clientIdType, clientId, knownFact)
      })({ fastTrack =>
        Some((fastTrack.service, fastTrack.clientIdentifierType, fastTrack.clientIdentifier, fastTrack.knownFact))
      }))
  }

  object ClientForMtdItWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[String] = arg match {
      case (UserInputNinoAndPostcode(HMRCMTDIT, Some(clientIdentifier), _), featureFlags)
          if featureFlags.showKfcMtdIt =>
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
      case (UserInputNinoAndPostcode(_, Some(_), _), featureFlags)
          if !featureFlags.showKfcMtdIt || !featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientForVatWithFlagOn {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[String] = arg match {
      case (UserInputVrnAndRegDate(HMRCMTDVAT, Some(clientIdentifier), _), featureFlags)
          if featureFlags.showKfcMtdVat =>
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
    def unapply(arg: CurrentInvitationInput)(implicit featureFlags: FeatureFlags): Option[FastTrackItsaInvitation] =
      arg match {
        case CurrentInvitationInput(Some(HMRCMTDIT), Some("ni"), Some(clientIdentifier), postcodeOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcMtdIt || postcodeOpt.exists(
              _.matches(postcodeRegex))) =>
          Some(
            FastTrackItsaInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcMtdIt) postcodeOpt.map(Postcode) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputPirReady {
    def unapply(arg: CurrentInvitationInput)(implicit featureFlags: FeatureFlags): Option[FastTrackPirInvitation] =
      arg match {
        case CurrentInvitationInput(Some(HMRCPIR), Some("ni"), Some(clientIdentifier), dobOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcPersonalIncome || dobOpt.exists(
              DateFieldHelper.validateDate)) =>
          Some(
            FastTrackPirInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcPersonalIncome) dobOpt.map(DOB) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputVatReady {
    def unapply(arg: CurrentInvitationInput)(implicit featureFlags: FeatureFlags): Option[FastTrackVatInvitation] =
      arg match {
        case CurrentInvitationInput(Some(HMRCMTDVAT), Some("vrn"), Some(clientIdentifier), vatRegDateOpt, _)
            if Vrn.isValid(clientIdentifier) && (!featureFlags.showKfcMtdVat || vatRegDateOpt.exists(
              DateFieldHelper.validateDate)) =>
          Some(
            FastTrackVatInvitation(
              Vrn(clientIdentifier),
              if (featureFlags.showKfcMtdVat) vatRegDateOpt.map(VatRegDate) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputNeedsClientIdentifier {
    def unapply(currentInvitationInput: CurrentInvitationInput): Option[CurrentInvitationInput] =
      currentInvitationInput match {
        case CurrentInvitationInput(Some(service), _, Some(clientIdentifier), _, _) =>
          service match {
            case HMRCMTDVAT if !Vrn.isValid(clientIdentifier) =>
              Some(CurrentInvitationInput(Some(HMRCMTDVAT), Some("vrn"), None, None))
            case HMRCMTDIT if !Nino.isValid(clientIdentifier) =>
              Some(CurrentInvitationInput(Some(HMRCMTDIT), Some("ni"), None, None))
            case HMRCPIR if !Nino.isValid(clientIdentifier) =>
              Some(CurrentInvitationInput(Some(HMRCPIR), Some("ni"), None, None))
            case _ => None
          }
        case CurrentInvitationInput(Some(service), _, None, _, _) =>
          Some(CurrentInvitationInput(service))
        case _ => None
      }
  }

  object CurrentInvitationInputNeedsKnownFact {
    def unapply(currentInvitationInput: CurrentInvitationInput): Option[CurrentInvitationInput] =
      currentInvitationInput match {
        case CurrentInvitationInput(Some(HMRCMTDVAT), Some(_), Some(_), Some(vatRegDate), _)
            if !DateFieldHelper.validateDate(vatRegDate) =>
          Some(currentInvitationInput.copy(knownFact = None))

        case CurrentInvitationInput(Some(HMRCMTDIT), Some(_), Some(_), Some(postcode), _)
            if !postcode.matches(postcodeRegex) =>
          Some(currentInvitationInput.copy(knownFact = None))

        case CurrentInvitationInput(Some(HMRCPIR), Some(_), Some(_), Some(dob), _)
            if !DateFieldHelper.validateDate(dob) =>
          Some(currentInvitationInput.copy(knownFact = None))

        case CurrentInvitationInput(Some(service), Some(_), Some(_), _, _)
            if Services.supportedServices.contains(service) =>
          Some(currentInvitationInput.copy(knownFact = None))

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
