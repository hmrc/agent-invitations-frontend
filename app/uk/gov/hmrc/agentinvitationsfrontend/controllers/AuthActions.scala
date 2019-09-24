/*
 * Copyright 2019 HM Revenue & Customs
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

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import play.api.{Configuration, Environment, Logger, Mode}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, AuthorisedClient}
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals._
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthActionsImpl @Inject()(
  val withVerifiedPasscode: PasscodeVerification,
  val externalUrls: ExternalUrls,
  val env: Environment,
  val config: Configuration,
  val authConnector: AuthConnector
) extends AuthActions

@ImplementedBy(classOf[AuthActionsImpl])
trait AuthActions extends AuthorisedFunctions with AuthRedirects {

  def withVerifiedPasscode: PasscodeVerification

  def externalUrls: ExternalUrls

  val isDevEnv: Boolean =
    if (env.mode.equals(Mode.Test)) false else config.getString("run.mode").forall(Mode.Dev.toString.equals)

  private val authenticationRedirect: String = config
    .getString("authentication.login-callback.url")
    .getOrElse(
      throw new IllegalStateException(s"No value found for configuration property: authentication.login-callback.url"))

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

  def withAuthorisedAsAnyClient[A](body: AuthorisedClient => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup and confidenceLevel and allEnrolments) {
        case Some(affinity) ~ confidence ~ enrols =>
          (affinity, confidence) match {
            case (AffinityGroup.Individual, cl) =>
              withConfidenceLevelUplift(cl, ConfidenceLevel.L200) {
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
        handleFailure(isAgent = false)
      }

  private def withConfidenceLevelUplift[A, BodyArgs](currentLevel: ConfidenceLevel, requiredLevel: ConfidenceLevel)(
    body: => Future[Result])(implicit request: Request[A]) =
    if (currentLevel >= requiredLevel) {
      body
    } else if (request.method == "GET") {
      redirectToIdentityVerification(requiredLevel)
    } else {
      Future.successful(Redirect(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url))
    }

  private def redirectToIdentityVerification[A](requiredLevel: ConfidenceLevel)(implicit request: Request[A]) = {
    val toLocalFriendlyUrl = CallOps.localFriendlyUrl(env, config) _
    val successUrl = toLocalFriendlyUrl(request.uri, request.host)
    val failureUrl =
      toLocalFriendlyUrl(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url, request.host)

    val ivUpliftUrl = CallOps.addParamsToUrl(
      personalIVUrl,
      "origin"          -> Some("aif"),
      "confidenceLevel" -> Some(requiredLevel.level.toString),
      "completionURL"   -> Some(successUrl),
      "failureURL"      -> Some(failureUrl)
    )

    Future.successful(Redirect(ivUpliftUrl))
  }

  def handleFailure(isAgent: Boolean)(implicit request: Request[_]): PartialFunction[Throwable, Result] = {
    case _: NoActiveSession ⇒
      toGGLogin(if (isDevEnv) s"http://${request.host}${request.uri}" else s"$authenticationRedirect${request.uri}")

    case _: InsufficientEnrolments ⇒
      Logger.warn(s"Logged in user does not have required enrolments")
      if (isAgent) Redirect(externalUrls.subscriptionURL) else Forbidden

    case _: UnsupportedAuthProvider ⇒
      Logger.warn(s"user logged in with unsupported auth provider")
      Forbidden
  }
}
