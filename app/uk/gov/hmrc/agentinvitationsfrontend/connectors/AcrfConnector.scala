/*
 * Copyright 2025 HM Revenue & Customs
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

import play.api.Logging
import play.api.http.Status.OK
import play.mvc.Http.HeaderNames
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import java.net.URLEncoder
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AcrfConnector @Inject() (http: HttpClient)(implicit val appConfig: AppConfig) extends Logging {

  def fastTrack(formData: Map[String, Seq[String]], continueUrl: Option[String], errorUrl: Option[String], hc: HeaderCarrier)(implicit
    ec: ExecutionContext
  ): Future[String] = {

    val queryStringParams = (continueUrl, errorUrl) match {
      case (Some(continue), Some(error)) =>
        s"?continue=${URLEncoder.encode(continue, "UTF-8")}&error=${URLEncoder.encode(error, "UTF-8")}"
      case _ => ""
    }

    val url = appConfig.fastTrackUrl + queryStringParams
    implicit val headerCarrierWithCookie: HeaderCarrier = hc.copy(extraHeaders = hc.headers(Seq(HeaderNames.COOKIE)))

    http.POSTForm(url, formData).map { response =>
      response.status match {
        case OK => appConfig.acrfBaseUrl + response.body
        case _ =>
          logger.warn("Unable to retrieve valid fast track URL, defaulting to start of journey")
          appConfig.createAuthRequestUrl
      }
    }
  }
}
