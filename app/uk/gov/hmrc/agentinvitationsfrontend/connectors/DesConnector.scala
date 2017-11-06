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
import javax.inject.{ Inject, Named, Singleton }

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import play.api.libs.json.{ JsValue, Json, Writes }
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentmtdidentifiers.model.{ Arn, MtdItId }
import uk.gov.hmrc.http.logging.Authorization
import uk.gov.hmrc.http.{ HeaderCarrier, HttpGet, HttpPost, HttpReads }

import scala.concurrent.{ ExecutionContext, Future }

case class RegistrationRelationshipResponse(processingDate: String)

object RegistrationRelationshipResponse {
  implicit val reads = Json.reads[RegistrationRelationshipResponse]
}

@Singleton
class DesConnector @Inject() (
  @Named("des-baseUrl") baseUrl: URL,
  @Named("des.authorizationToken") authorizationToken: String,
  @Named("des.environment") environment: String,
  httpGet: HttpGet,
  httpPost: HttpPost,
  metrics: Metrics)
  extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def createAgentRelationship(mtdbsa: MtdItId, arn: Arn)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[RegistrationRelationshipResponse] = {
    val url = new URL(baseUrl, s"/registration/relationship")
    postWithDesHeaders[JsValue, RegistrationRelationshipResponse]("CreateAgentRelationship", url, createAgentRelationshipInputJson(mtdbsa.value, arn.value))
  }

  private def createAgentRelationshipInputJson(refNum: String, agentRefNum: String) = Json.parse(
    s"""{
         "acknowledgmentReference": "${java.util.UUID.randomUUID().toString.replace("-", "").take(32)}",
          "refNumber": "$refNum",
          "agentReferenceNumber": "$agentRefNum",
          "regime": "ITSA",
          "authorisation": {
            "action": "Authorise",
            "isExclusiveAgent": true
          }
       }""")

  private def postWithDesHeaders[A: Writes, B: HttpReads](apiName: String, url: URL, body: A)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[B] = {
    val desHeaderCarrier = hc.copy(
      authorization = Some(Authorization(s"Bearer $authorizationToken")),
      extraHeaders = hc.extraHeaders :+ "Environment" -> environment)
    monitor(s"ConsumedAPI-DES-$apiName-POST") {
      httpPost.POST[A, B](url.toString, body)(implicitly[Writes[A]], implicitly[HttpReads[B]], desHeaderCarrier, ec)
    }
  }

}