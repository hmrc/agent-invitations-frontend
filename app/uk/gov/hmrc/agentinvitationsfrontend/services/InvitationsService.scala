/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.services

import java.net.URL
import javax.inject.{Inject, Singleton}
import cats.data.OptionT
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, AgentInvitationUserInput, ClientInvitation, Invitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

@Singleton
class InvitationsService @Inject()(invitationsConnector: InvitationsConnector) {

  // TODO handle 403 CLIENT_REGISTRATION_NOT_FOUND response (happens when no client found for given known facts)
  // currently this results in an error page, we should give the user a nicer message
//  def createInvitation(arn: Arn, userInput: AgentInvitationUserInput)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Invitation]] = {
//    val agentInvitation = AgentInvitation(
//      "HMRC-MTD-IT",
//      "ni",
//      userInput.nino.value,
//      userInput.postcode
//    )
//
//    (for {
//      location <- OptionT(invitationsConnector.createInvitation(arn, agentInvitation))
//      invitation <- OptionT(getInvitation(location))
//    } yield {
//      invitation
//    }) value
//  }

//  def getPendingClientInvitations(mtdItId: MtdItId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[ClientInvitation]] = {
//    invitationsConnector.getPendingClientInvitations(mtdItId)
//      .map {
//        _.map(makeClientInvitation)
//      }
//  }

//  def getInvitation(location: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Invitation]] = {
//    invitationsConnector.getInvitation(location)
//  }
//
//  def viewInvitation(mtdItId: MtdItId, invitationId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[ClientInvitation]] = {
//    invitationsConnector.viewInvitation(mtdItId, invitationId).map {
//      case Some(invitation) => Some(makeClientInvitation(invitation))
//      case None => None
//    }
//  }

//  def actionInvitation(invitationUrl: URL, actionUrl: URL)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[ClientInvitation] = {
//    invitationsConnector.transitionClientInvitation(actionUrl) flatMap { _ =>
//      invitationsConnector.getInvitation(invitationUrl) map {
//        case Some(invitation) => makeClientInvitation(invitation)
//        case None => throw new RuntimeException(s"$invitationUrl does not refer to an invitation")
//      }
//    }
//  }

//  private def makeClientInvitation(i: Invitation)(implicit hc: HeaderCarrier): ClientInvitation =
//    ClientInvitation(
//      self = i.selfUrl,
//      arn = i.arn,
//      service = i.service,
//      status = i.status,
//      created = i.created,
//      lastUpdated = i.lastUpdated,
//      acceptUrl = i.acceptUrl,
//      rejectUrl = i.rejectUrl
//    )
}
