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

import javax.inject.Inject
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

class AgentSuspensionConnector @Inject()(http: HttpClient)(implicit appConfig: AppConfig) {

  def getSuspendedServices(arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SuspensionResponse] = {
    val url = s"${appConfig.agentSuspensionBaseUrl}/agent-suspension/status/arn/${arn.value}"
    http.GET[SuspensionResponse](url)
  } recoverWith {
    case _: NotFoundException => Future successful SuspensionResponse(Set.empty)
  }
}

case class SuspensionResponse(services: Set[String]) {

  def getSuspendedServices(s: Set[String]): Set[String] = s.intersect(services)

  def isAllSuspended(s: Set[String]): Boolean =
    s.diff(services) == Set.empty

  def isSuspended(s: Set[String]): Boolean = getSuspendedServices(s).nonEmpty

  def isSuspendedService(s: String): Boolean = services.contains(s)

}

object SuspensionResponse {
  implicit val formats: OFormat[SuspensionResponse] = Json.format
}
