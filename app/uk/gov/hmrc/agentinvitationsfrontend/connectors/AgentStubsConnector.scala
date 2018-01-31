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

package uk.gov.hmrc.agentinvitationsfrontend.connectors

import java.net.URL
import javax.inject.{Inject, Named, Singleton}

import com.kenshoo.play.metrics.Metrics
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, HttpGet, NotFoundException}
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

case class RegisteredClient(vrn: String, registrationDate: String)

object RegisteredClient {
  implicit val formats = Json.format[RegisteredClient]
}

@Singleton
class AgentStubsConnector @Inject() (@Named("agent-stubs-baseUrl") baseUrl: URL,
                                     http: HttpGet,
                                     metrics: Metrics) {
  def getVatRegisteredClient(vrn: String)(implicit hc: HeaderCarrier): Future[Option[RegisteredClient]] =
    http.GET[RegisteredClient](new URL(baseUrl, s"/clients/registrationDate/vrn/$vrn").toString).map{ reg =>
      Some(reg)
    }.recover{
      case e: NotFoundException => None
      case ex @ _ => throw new RuntimeException(s"Something went wrong with AgentStubs: $ex")
    }
}
