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

package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL

import javax.inject.{Inject, Named}
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.{HeaderCarrier, HttpGet, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}

case class AgentSuspensionResponse(status: String, suspendedServices: Option[Set[String]] = None)

case class AgentSuspensionStatusNotFound() extends Exception

object AgentSuspensionResponse {
  implicit val formats: OFormat[AgentSuspensionResponse] = Json.format
}

class AgentSuspensionConnector @Inject()(@Named("agent-suspension-baseUrl") baseUrl: URL, http: HttpGet) {

  def getSuspensionStatus(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[AgentSuspensionResponse] = {
    http.GET[AgentSuspensionResponse](new URL(baseUrl, s"/agent-suspension/status/arn/${arn.value}").toString)
  } recoverWith {
    case _: NotFoundException => Future failed AgentSuspensionStatusNotFound()
  }
}
