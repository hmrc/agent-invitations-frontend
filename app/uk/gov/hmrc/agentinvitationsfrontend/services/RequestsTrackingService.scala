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

package uk.gov.hmrc.agentinvitationsfrontend.services

import javax.inject.{Inject, Singleton}
import org.joda.time.LocalDate
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, CitizenDetailsConnector, InvitationsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{Services, StoredInvitation, TrackedInvitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class RequestsTrackingService @Inject()(
  invitationsConnector: InvitationsConnector,
  val agentServicesAccountConnector: AgentServicesAccountConnector,
  val citizenDetailsConnector: CitizenDetailsConnector)
    extends GetClientName {

  def getRecentAgentInvitations(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[TrackedInvitation]] =
    invitationsConnector
      .getAllInvitations(arn, LocalDate.now().minusDays(showLastDays))
      .map(_.filter(whitelistedInvitation(isPirWhitelisted)).map(TrackedInvitation.apply))
      .flatMap(ii => Future.sequence(ii.map(addClientName)))

  def whitelistedInvitation(isPirWhitelisted: Boolean): StoredInvitation => Boolean =
    i => isPirWhitelisted || i.service != Services.HMRCPIR

  def addClientName(
    invitation: TrackedInvitation)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[TrackedInvitation] =
    getClientNameByService(invitation).map(cn => invitation.copy(clientName = cn))

}
