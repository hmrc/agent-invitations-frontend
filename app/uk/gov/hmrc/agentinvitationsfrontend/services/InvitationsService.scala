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

import javax.inject.{Inject, Singleton}

import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, AgentInvitationUserInput, Invitation}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class InvitationsService @Inject()(invitationsConnector: InvitationsConnector) {

  def createInvitation(arn: Arn, userInput: AgentInvitationUserInput)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Invitation]] = {

    val agentInvitation = AgentInvitation("HMRC-MTD-IT", "ni", userInput.nino.value, userInput.postcode)

    for {
      location <- invitationsConnector.createInvitation(arn, agentInvitation)
      invitation <- invitationsConnector.getInvitation(location.getOrElse(""))
    } yield invitation
  }

  def getInvitation(location: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Invitation]] =
    invitationsConnector.getInvitation(location)

}
