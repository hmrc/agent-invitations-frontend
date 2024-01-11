/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.refactored.connectors

import play.api.http.Status.{INTERNAL_SERVER_ERROR, OK}
import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentservice.{CustomerDataCheckRequest, CustomerDataCheckResponse}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.http.client.HttpClientV2

import java.net.URL
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class HodConnector @Inject()(httpV2: HttpClientV2, appConfig: AppConfig) {

  private val acaBaseUrl = appConfig.agentClientAuthorisationBaseUrl

  def makePostRequest(service: String)(clientId: String, mKnownFact: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext) = {

    val url = new URL(s"$acaBaseUrl/agent-client-authorisation/test-only/customer-data-check/service/$service/client/$clientId")

    httpV2
      .post(url)
      .withBody(Json.toJson(CustomerDataCheckRequest(mKnownFact)))
      .execute[CustomerDataCheckResponse]

  }

}
