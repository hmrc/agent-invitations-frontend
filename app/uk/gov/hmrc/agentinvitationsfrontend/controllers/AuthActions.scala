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
import play.api.{Configuration, Environment, Logger}
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

  private def getEnrolmentValue(enrolments: Enrolments, serviceName: String, identifierKey: String) =
    for {
      enrolment  <- enrolments.getEnrolment(serviceName)
      identifier <- enrolment.getIdentifier(identifierKey)
    } yield identifier.value

  def withAuthorisedAsAgent[A](body: AuthorisedAgent => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    withVerifiedPasscode { isWhitelisted =>
      withEnrolledAsAgent {
        case Some(arn) =>
          body(AuthorisedAgent(Arn(arn), isWhitelisted))
        case None => Future.failed(InsufficientEnrolments("AgentReferenceNumber identifier not found"))
      } recoverWith {
        case _: InsufficientEnrolments => Future successful Redirect(externalUrls.subscriptionURL)
      }
    }

  def withAuthorisedAsAnyClient[A](body: AuthorisedClient => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(
      AuthProviders(GovernmentGateway)
    ).retrieve(affinityGroup and confidenceLevel and allEnrolments) {
        case Some(affinity) ~ confidence ~ enrols =>
          (affinity, confidence) match {
            case (AffinityGroup.Individual, cl) =>
              withConfidenceLevelUplift(cl, ConfidenceLevel.L200) {
                body(AuthorisedClient(affinity, enrols))
              }
            case (AffinityGroup.Organisation, _) => body(AuthorisedClient(affinity, enrols))
            case (AffinityGroup.Agent, _) =>
              Future successful Redirect(routes.ClientInvitationJourneyController.incorrectlyAuthorisedAsAgent())
          }

        case _ =>
          Logger.warn("the user had no affinity group")
          Future successful Forbidden("user has no affinity group")
      }
      .recover {
        case ex: AuthorisationException =>
          Logger.error(s"authorisation failed for reason: ${ex.reason}")
          throw ex
      }

  def withEnrolledAsAgent[A](body: Option[String] => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(
      Enrolment("HMRC-AS-AGENT")
        and AuthProviders(GovernmentGateway))
      .retrieve(authorisedEnrolments) { enrolments =>
        val id = getEnrolmentValue(enrolments, "HMRC-AS-AGENT", "AgentReferenceNumber")
        body(id)
      }

  private def withConfidenceLevelUplift[A, BodyArgs](currentLevel: ConfidenceLevel, requiredLevel: ConfidenceLevel)(
    body: => Future[Result])(implicit request: Request[A]) =
    if (currentLevel >= requiredLevel) {
      body
    } else if (request.method == "GET") {
      redirectToIdentityVerification(requiredLevel)
    } else {
      Future.successful(Redirect(routes.ClientInvitationJourneyController.incorrectlyAuthorisedAsAgent().url))
    }

  private def redirectToIdentityVerification[A](requiredLevel: ConfidenceLevel)(implicit request: Request[A]) = {
    val toLocalFriendlyUrl = CallOps.localFriendlyUrl(env, config) _
    val successUrl = toLocalFriendlyUrl(request.uri, request.host)
    val failureUrl =
      toLocalFriendlyUrl(routes.ClientInvitationJourneyController.incorrectlyAuthorisedAsAgent().url, request.host)

    val ivUpliftUrl = CallOps.addParamsToUrl(
      personalIVUrl,
      "origin"          -> Some("aif"),
      "confidenceLevel" -> Some(requiredLevel.level.toString),
      "completionURL"   -> Some(successUrl),
      "failureURL"      -> Some(failureUrl)
    )

    Future.successful(Redirect(ivUpliftUrl))
  }
}
