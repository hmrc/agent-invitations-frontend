/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import play.api.{Configuration, Environment, Logging}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.PirRelationshipConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, AuthorisedClient, Services}
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps
import uk.gov.hmrc.agentinvitationsfrontend.support.CallOps._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals._
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.AuthRedirects

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthActionsImpl @Inject()(
  val externalUrls: ExternalUrls,
  val env: Environment,
  val config: Configuration,
  val authConnector: AuthConnector,
  val appConfig: AppConfig
) extends AuthorisedFunctions with AuthRedirects with Logging {

  private val requiredCL = ConfidenceLevel.L200

  private def getArn(enrolments: Enrolments) =
    for {
      enrolment  <- enrolments.getEnrolment("HMRC-AS-AGENT")
      identifier <- enrolment.getIdentifier("AgentReferenceNumber")
    } yield Arn(identifier.value)

  def withAuthorisedAsAnyAgent[A](body: => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup) {
        case Some(AffinityGroup.Agent) => body
        case _ =>
          logger.warn(s"problem retrieving affinity group $affinityGroup")
          Future successful Forbidden
      }
      .recover {
        handleFailure(isAgent = true)
      }

  def withAuthorisedAsAgent[A](
    body: AuthorisedAgent => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    authorised(Enrolment("HMRC-AS-AGENT") and AuthProviders(GovernmentGateway))
      .retrieve(authorisedEnrolments) { enrolments =>
        getArn(enrolments) match {
          case Some(arn) => body(AuthorisedAgent(arn))
          case None =>
            logger.warn("Arn not found for the logged in agent")
            Future successful Forbidden
        }
      }
      .recover {
        handleFailure(isAgent = true)
      }

  def withIndividualAuth[A](body: String => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup and credentials) {
        case Some(affinity) ~ Some(Credentials(providerId, _)) =>
          if (affinity == AffinityGroup.Individual) {
            body(providerId)
          } else {
            logger.warn(s"affinity group: $affinityGroup is not individual cannot progress")
            Future successful Forbidden
          }
        case _ =>
          logger.warn(s"problem retrieving affinity group $affinityGroup or credentials")
          Future successful Forbidden
      }
      .recover {
        handleFailure(isAgent = false)
      }

  def withAuthorisedAsAnyClient[A](journeyId: Option[String])(
    body: AuthorisedClient => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup and confidenceLevel and allEnrolments and nino) {
        case Some(affinity) ~ confidence ~ enrols ~ maybeNino =>
          (affinity, confidence) match {
            case (AffinityGroup.Individual, cl) =>
              withConfidenceLevelUplift(cl, enrols) {
                body(AuthorisedClient(affinity, enrols))
              }
            case (AffinityGroup.Organisation, cl) => {
              if (enrols.enrolments.map(_.key).contains(Services.HMRCMTDIT)) withConfidenceLevelUplift(cl, enrols) {
                body(AuthorisedClient(affinity, enrols))
              } else body(AuthorisedClient(affinity, enrols))
            }
            case (AffinityGroup.Agent, _) => {
              Future successful Redirect(routes.ClientInvitationJourneyController.showErrorCannotViewRequest())
            }
            case (affinityGroup, _) =>
              logger.warn(s"unknown affinity group: $affinityGroup - cannot determine auth status")
              Future successful Forbidden
          }

        case _ =>
          logger.warn("the logged in client had no affinity group")
          Future successful Forbidden
      }
      .recover {
        handleFailure(isAgent = false, journeyId)
      }

  def withMaybeLoggedInClient[A](
    body: Option[AuthorisedClient] => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    authorised(AuthProviders(GovernmentGateway))
      .retrieve(affinityGroup and confidenceLevel and allEnrolments and nino) {
        case Some(affinity) ~ confidence ~ enrols ~ maybeNino =>
          (affinity, confidence) match {
            case (AffinityGroup.Individual, cl) =>
              withConfidenceLevelUplift(cl, enrols) {
                body(Some(AuthorisedClient(affinity, enrols)))
              }
            case (AffinityGroup.Organisation, cl) => {
              if (enrols.enrolments.map(_.key).contains(Services.HMRCMTDIT)) withConfidenceLevelUplift(cl, enrols) {
                body(Some(AuthorisedClient(affinity, enrols)))
              } else body(Some(AuthorisedClient(affinity, enrols)))
            }
            case (AffinityGroup.Agent, _) => {
              Future successful Redirect(routes.ClientInvitationJourneyController.showErrorCannotViewRequest())
            }
            case (affinityGroup, _) =>
              logger.warn(s"unknown affinity group: $affinityGroup - cannot determine auth status")
              Future successful Forbidden
          }

        case _ =>
          logger.warn("the logged in client had no affinity group")
          Future successful Forbidden
      }
      .recoverWith {
        case _: NoActiveSession ⇒
          body(None)

        case _: InsufficientEnrolments ⇒
          logger.warn(s"Logged in user does not have required enrolments")
          Future.successful(Forbidden)

        case _: UnsupportedAuthProvider ⇒
          logger.warn(s"user logged in with unsupported auth provider")
          Future.successful(Forbidden)
      }

  private def withConfidenceLevelUplift[A, BodyArgs](currentLevel: ConfidenceLevel, enrols: Enrolments)(body: => Future[Result])(
    implicit request: Request[A]): Future[Result] = {

    //APB-4856: Clients with only CGT enrol dont need to go through IV
    val isCgtOnlyClient: Boolean = {
      val enrolKeys: Set[String] = enrols.enrolments.map(_.key)
      enrolKeys.intersect(Services.supportedEnrolmentKeys) == Set(Services.HMRCCGTPD)
    }

    if (currentLevel >= requiredCL || isCgtOnlyClient) {
      body
    } else if (request.method == "GET") {
      redirectToIdentityVerification()
    } else {
      Future.successful(Redirect(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url))
    }
  }

  private def redirectToIdentityVerification[A]()(implicit request: Request[A]) = {
    val toLocalFriendlyUrl = CallOps.localFriendlyUrl(env) _
    val successUrl = toLocalFriendlyUrl(request.uri, request.host)
    val rawFailureUrl =
      toLocalFriendlyUrl(routes.ClientInvitationJourneyController.showCannotConfirmIdentity().url, request.host)

    //add success url to params so that when the user succeeds after failing they can continue their journey.
    val failureUrl = CallOps.addParamsToUrl(rawFailureUrl, "success" -> Some(successUrl))

    val ivUpliftUrl = CallOps.addParamsToUrl(
      personalIVUrl,
      "origin"          -> Some("aif"),
      "confidenceLevel" -> Some(requiredCL.toString),
      "completionURL"   -> Some(successUrl),
      "failureURL"      -> Some(failureUrl)
    )
    Future.successful(Redirect(ivUpliftUrl))
  }

  private def continueUrlWithJourneyId(journeyId: Option[String])(implicit request: Request[_]): String = {
    val url = s"$continueUrl${request.uri}"
    journeyId.fold(url)(_ => addParamsToUrl(url, "clientInvitationJourney" -> journeyId))
  }

  def handleFailure(isAgent: Boolean, journeyId: Option[String] = None)(implicit request: Request[_]): PartialFunction[Throwable, Result] = {
    case _: NoActiveSession ⇒
      Redirect(s"$signInUrl?origin=${getString("appName")}&continue_url=${continueUrlWithJourneyId(journeyId)}")

    case _: InsufficientEnrolments ⇒
      logger.warn(s"Logged in user does not have required enrolments")
      if (isAgent) Redirect(externalUrls.subscriptionURL) else Forbidden

    case _: UnsupportedAuthProvider ⇒
      logger.warn(s"user logged in with unsupported auth provider")
      Forbidden
  }

  private def getString(key: String): String = config.underlying.getString(key)

  private val signInUrl = getString("bas-gateway.url")
  private val continueUrl = getString("login.continue")
}
