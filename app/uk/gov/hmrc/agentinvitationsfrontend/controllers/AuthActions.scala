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

import play.api.Logger
import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Retrievals._
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthActions extends AuthorisedFunctions {

  def withVerifiedPasscode: PasscodeVerification

  def externalUrls: ExternalUrls

  private def getEnrolmentValue(enrolments: Enrolments, serviceName: String, identifierKey: String) =
    for {
      enrolment  <- enrolments.getEnrolment(serviceName)
      identifier <- enrolment.getIdentifier(identifierKey)
    } yield identifier.value

  protected def withAuthorisedAsAgent[A](body: (Arn, Boolean) => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    withVerifiedPasscode { isWhitelisted =>
      withEnrolledAsAgent {
        case Some(arn) =>
          body(Arn(arn), isWhitelisted)
        case None => Future.failed(InsufficientEnrolments("AgentReferenceNumber identifier not found"))
      } recoverWith {
        case _: InsufficientEnrolments => Future successful Redirect(externalUrls.subscriptionURL)
      }
    }

  protected def withAuthorisedAsClient[A](serviceName: String, identifierKey: String)(body: String => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    withEnrolledAsClient(serviceName, identifierKey) {
      case Some(clientId) => body(clientId)
      case None           => Future.failed(InsufficientEnrolments(s"$identifierKey identifier not found"))
    }.recover {
      case _: InsufficientEnrolments =>
        serviceName match {
          case Services.HMRCNI => Redirect(routes.ClientsInvitationController.notAuthorised())
          case _ =>
            Redirect(routes.ClientsInvitationController.notSignedUp())
              .addingToSession("clientService" -> serviceName)
        }
      case _: InsufficientConfidenceLevel =>
        Redirect(routes.ClientsInvitationController.notFoundInvitation())

    }

  private def extractAffinityGroup(affinityGroup: AffinityGroup): String =
    (affinityGroup.toJson \ "affinityGroup").as[String]

  protected def withAuthorisedAsAnyClient[A](body: (String, Seq[(String, String)]) => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(
      AuthProviders(GovernmentGateway) and (AffinityGroup.Individual or AffinityGroup.Organisation)
    ).retrieve(affinityGroup and confidenceLevel and allEnrolments) {
        case Some(affinity) ~ confidence ~ enrols =>
          val affinityG: String = extractAffinityGroup(affinity)
          val clientIdTypePlusIds: Seq[(String, String)] = enrols.enrolments.map { enrolment =>
            (enrolment.identifiers.head.key, enrolment.identifiers.head.value.replaceAll(" ", ""))
          }.toSeq
          (affinity, confidence) match {
            case (AffinityGroup.Individual, ConfidenceLevel.L200) => body(affinityG, clientIdTypePlusIds)
            case (AffinityGroup.Organisation, _)                  => body(affinityG, clientIdTypePlusIds)
            case _                                                => Future successful Redirect(routes.ClientsInvitationController.notAuthorised())
          }
        case _ => Future successful Redirect(routes.ClientsInvitationController.notAuthorised())
      }
      .recover {
        case _: InsufficientEnrolments =>
          Redirect(routes.ClientsInvitationController.notAuthorised())
        case _: InsufficientConfidenceLevel =>
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
      }

  protected def withEnrolledAsAgent[A](body: Option[String] => Future[Result])(
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

  private val authLoginCredentials
    : Retrieval[Option[AffinityGroup] ~ Enrolments ~ ConfidenceLevel] = affinityGroup and authorisedEnrolments and confidenceLevel

  private def extractEnrolmentsValue(enrolments: Enrolments, serviceName: String, identifierKey: String) =
    for {
      enrolment  <- enrolments.getEnrolment(serviceName)
      identifier <- enrolment.getIdentifier(identifierKey)
    } yield identifier.value

  protected def withEnrolledAsClient[A](serviceName: String, identifierKey: String)(
    body: Option[String] => Future[Result])(
    implicit request: Request[A],
    hc: HeaderCarrier,
    ec: ExecutionContext): Future[Result] =
    authorised(
      Enrolment(serviceName)
        and AuthProviders(GovernmentGateway)
    ).retrieve(authLoginCredentials) {
      case affinity ~ enrols ~ confidence =>
        val id = extractEnrolmentsValue(enrols, serviceName, identifierKey)
        (affinity, confidence) match {
          case (Some(AffinityGroup.Organisation), _)                  => body(id)
          case (Some(AffinityGroup.Individual), ConfidenceLevel.L200) => body(id)
          case (Some(AffinityGroup.Agent), _)                         => body(None)
          case _ =>
            Future failed (throw InsufficientConfidenceLevel(
              s"Client Logged in did not have sufficient confidence level for $serviceName"))
        }
      case _ =>
        Logger(getClass).warn("Invalid Login")
        body(None)
    }
}
