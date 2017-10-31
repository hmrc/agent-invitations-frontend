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

package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL
import javax.inject.{Inject, Named}

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.UriPathEncoding.encodePathSegment
import uk.gov.hmrc.agentinvitationsfrontend.models.{AgentInvitation, Invitation}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId}
import uk.gov.hmrc.http.{HeaderCarrier, HttpGet, HttpPost, HttpResponse}
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

class InvitationsConnector @Inject() (@Named("agent-client-authorisation-baseUrl") baseUrl: URL, http: HttpGet with HttpPost, metrics: Metrics) extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  private[connectors] def createInvitationUrl(arn: Arn) = new URL(baseUrl, s"/agent-client-authorisation/agencies/${encodePathSegment(arn.value)}/invitations/sent")
  private def invitationUrl(location: String) = new URL(baseUrl, location)
  private[connectors] def viewInvitationUrl(mtdItId: MtdItId, invitationId: String) =
    new URL(baseUrl, s"/agent-client-authorisation/clients/MTDITID/${encodePathSegment(mtdItId.value)}/invitations/received/$invitationId")



  def createInvitation(arn: Arn, agentInvitation: AgentInvitation)(implicit hc: HeaderCarrier): Future[Option[String]] = {
    http.POST[AgentInvitation, HttpResponse](createInvitationUrl(arn).toString, agentInvitation) map { r =>
      r.header("location")
    }
  }

  def getInvitation(location: String)(implicit hc: HeaderCarrier): Future[Option[Invitation]] = {
    getInvitation(invitationUrl(location))
  }

  def getInvitation(url: URL)(implicit hc: HeaderCarrier): Future[Option[Invitation]] = {
    implicit val readsInvitation = Invitation.reads(url)
    http.GET[Option[Invitation]](url.toString)
  }

  def viewInvitation(mtdItId: MtdItId, invitationId: String)(implicit hc: HeaderCarrier): Future[Option[Invitation]] = {
    val url = viewInvitationUrl(mtdItId, invitationId)
    implicit val readInvitation = Invitation.reads(url)
    http.GET[Option[Invitation]](url.toString)
  }

}
