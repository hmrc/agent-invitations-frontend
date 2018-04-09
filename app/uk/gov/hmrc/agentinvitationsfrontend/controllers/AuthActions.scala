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

import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.Retrievals.authorisedEnrolments
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthActions extends AuthorisedFunctions {

  def withVerifiedPasscode: PasscodeVerification

  def externalUrls: ExternalUrls

  private def getEnrolmentValue(enrolments: Enrolments, serviceName: String, identifierKey: String) = for {
    enrolment <- enrolments.getEnrolment(serviceName)
    identifier <- enrolment.getIdentifier(identifierKey)
  } yield identifier.value


  protected def withAuthorisedAsAgent[A](body: (Arn, Boolean) => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    withVerifiedPasscode { isWhitelisted =>
      withEnrolledAsAgent {
        case Some(arn) =>
          body(Arn(arn), isWhitelisted)
        case None => Future.failed(InsufficientEnrolments("AgentReferenceNumber identifier not found"))
      } recoverWith {
        case _: InsufficientEnrolments => Future successful Redirect(externalUrls.subscriptionURL)
      }
    }

  protected def withAuthorisedAsClient[A](serviceName: String, identifierKey: String)(body: String => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    withEnrolledAsClient(serviceName, identifierKey) {
      case Some(clientId) => body(clientId)
      case None => Future.failed(InsufficientEnrolments(s"$identifierKey identifier not found"))
    }.recover {
      case _: UnsupportedAffinityGroup =>
        Redirect(routes.ClientsInvitationController.notAuthorised())
      case _: InsufficientEnrolments =>
        serviceName match {
          case Services.HMRCNI => Redirect(routes.ClientsInvitationController.notAuthorised())
          case _ => Redirect(routes.ClientsInvitationController.notSignedUp())
        }
      case _: InsufficientConfidenceLevel =>
        Redirect(routes.ClientsInvitationController.notFoundInvitation())

    }

  protected def withEnrolledAsAgent[A](body: Option[String] => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    authorised(
      Enrolment("HMRC-AS-AGENT")
        and AuthProviders(GovernmentGateway))
      .retrieve(authorisedEnrolments) { enrolments =>
        val id = getEnrolmentValue(enrolments, "HMRC-AS-AGENT", "AgentReferenceNumber")
        body(id)
      }
  }

  val affinityGroupForService: String => Predicate = {
    case Services.HMRCNI => AffinityGroup.Individual
    case Services.HMRCPIR => AffinityGroup.Individual
    case Services.HMRCMTDIT => AffinityGroup.Individual or AffinityGroup.Organisation
    case Services.HMRCMTDVAT => AffinityGroup.Organisation
  }

  protected def withEnrolledAsClient[A](serviceName: String, identifierKey: String)(body: Option[String] => Future[Result])(implicit request: Request[A], hc: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    authorised(
      Enrolment(serviceName)
        and AuthProviders(GovernmentGateway)
        and ConfidenceLevel.L200
        and affinityGroupForService(serviceName)
    )
      .retrieve(authorisedEnrolments) { enrolments =>
        val id = for {
          enrolment <- enrolments.getEnrolment(serviceName)
          identifier <- enrolment.getIdentifier(identifierKey)
        } yield identifier.value

        body(id)
      }
  }
}
