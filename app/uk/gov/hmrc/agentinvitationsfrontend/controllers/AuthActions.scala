/*
 * Copyright 2020 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}
import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, AuthorisedClient}
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthActionsImpl @Inject()(
  val withVerifiedPasscode: PasscodeVerification,
  val externalUrls: ExternalUrls,
  val env: Environment,
  val config: Configuration,
  val authConnector: AuthConnector,
  val appConfig: AppConfig)
    extends AuthorisedFunctions with AuthRedirects {

  val pdvStartUrl = s"${externalUrls.pdvFrontendUrl}/start"

  val isDevEnv: Boolean =
    if (env.mode.equals(Mode.Test)) false else appConfig.runMode.env.contains("Dev")

  private def getArn(enrolments: Enrolments) =
    for {
      enrolment  <- enrolments.getEnrolment("HMRC-AS-AGENT")
      identifier <- enrolment.getIdentifier("AgentReferenceNumber")
    } yield Arn(identifier.value)

  def withAuthorisedAsAgent[A](body: AuthorisedAgent => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    withVerifiedPasscode { isWhitelisted =>
      authorised(Enrolment("HMRC-AS-AGENT") and AuthProviders(GovernmentGateway))
        .retrieve(authorisedEnrolments) { enrolments =>
          getArn(enrolments) match {
            case Some(arn) => body(AuthorisedAgent(arn, isWhitelisted))
            case None =>
              Logger.warn("Arn not found for the logged in agent")
              Future successful Forbidden
          }
        }
        .recover {
          handleFailure(isAgent = true)
        }
    }

  def withIndividualAuth[A](body: String => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup and credentials) {
        case Some(affinity) ~ Some(Credentials(providerId, _)) =>
          if (affinity == AffinityGroup.Individual) {
            body(providerId)
          } else {
            Logger.warn(s"affinity group: $affinityGroup is not individual cannot progress")
            Future successful Forbidden
          }
        case _ =>
          Logger.warn(s"problem retrieving affinity group $affinityGroup or credentials")
          Future successful Forbidden
      }
      .recover {
        handleFailure(isAgent = false)
      }

  def withAuthorisedAsAnyClient[A](journeyId: Option[String])(body: AuthorisedClient => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup and confidenceLevel and allEnrolments and nino) {
        case Some(affinity) ~ confidence ~ enrols ~ maybeNino =>
          (affinity, confidence) match {
            case (AffinityGroup.Individual, cl) =>
              withConfidenceLevelUplift(cl, ConfidenceLevel.L200, journeyId, maybeNino) {
                body(AuthorisedClient(affinity, enrols))
              }
            case (AffinityGroup.Organisation, _) => body(AuthorisedClient(affinity, enrols))
            case (AffinityGroup.Agent, _) =>
              Future successful Redirect(routes.ClientInvitationJourneyController.incorrectlyAuthorisedAsAgent())
            case (affinityGroup, _) =>
              Logger.warn(s"unknown affinity group: $affinityGroup - cannot determine auth status")
              Future successful Forbidden
          }

        case _ =>
          Logger.warn("the logged in client had no affinity group")
          Future successful Forbidden
      }
      .recover {
        handleFailure(isAgent = false, journeyId)
      }

  private def withConfidenceLevelUplift[A, BodyArgs](
    currentLevel: ConfidenceLevel,
    requiredLevel: ConfidenceLevel,
    journeyId: Option[String],
    mayBeNino: Option[String])(body: => Future[Result])(implicit request: Request[A]): Future[Result] =
    if (currentLevel >= requiredLevel) {
      body
    } else if (request.method == "GET" && mayBeNino.isDefined) {
      redirectToIdentityVerification(requiredLevel)
    } else if (request.method == "GET") {
      redirectToPersonalDetailsValidation()
    } else {
      Future.successful(Redirect(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url))
    }

  private def redirectToIdentityVerification[A](requiredLevel: ConfidenceLevel)(implicit request: Request[A]) = {
    val toLocalFriendlyUrl = CallOps.localFriendlyUrl(env, appConfig) _
    val successUrl = toLocalFriendlyUrl(request.uri, request.host)
    val rawFailureUrl =
      toLocalFriendlyUrl(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url, request.host)

    //add success url to params so that when the user succeeds after failing they can continue their journey.
    val failureUrl = CallOps.addParamsToUrl(rawFailureUrl, "success" -> Some(successUrl))

    val ivUpliftUrl = CallOps.addParamsToUrl(
      personalIVUrl,
      "origin"          -> Some("aif"),
      "confidenceLevel" -> Some(requiredLevel.level.toString),
      "completionURL"   -> Some(successUrl),
      "failureURL"      -> Some(failureUrl)
    )
    Future.successful(Redirect(ivUpliftUrl))
  }

  private def redirectToPersonalDetailsValidation[A]()(implicit request: Request[A]): Future[Result] = {

    val toLocalFriendlyUrl = CallOps.localFriendlyUrl(env, appConfig) _

    val targetUrl = toLocalFriendlyUrl(request.uri, request.host)
    val completeUrlBase = toLocalFriendlyUrl(routes.ClientInvitationJourneyController.pdvComplete().url, request.host)

    // completion URL needs to include the target (where user is trying to go)
    val pdvCompleteUrl =
      CallOps.addParamsToUrl(completeUrlBase, "targetUrl" -> Some(targetUrl))

    val personalDetailsValidationUrl =
      CallOps.addParamsToUrl(pdvStartUrl, "completionUrl" -> Some(pdvCompleteUrl))

    Future successful Redirect(personalDetailsValidationUrl)
  }

  def handleFailure(isAgent: Boolean, journeyId: Option[String] = None)(
    implicit request: Request[_]): PartialFunction[Throwable, Result] = {
    case _: NoActiveSession ⇒ {
      val url = localFriendlyUrl(env, appConfig)(request.uri, request.host)
      val ggContinueUrl = journeyId.fold(url)(_ => addParamsToUrl(url, "clientInvitationJourney" -> journeyId))
      toGGLogin(ggContinueUrl)
    }

    case _: InsufficientEnrolments ⇒
      Logger.warn(s"Logged in user does not have required enrolments")
      if (isAgent) Redirect(externalUrls.subscriptionURL) else Forbidden

    case _: UnsupportedAuthProvider ⇒
      Logger.warn(s"user logged in with unsupported auth provider")
      Forbidden
  }
}
