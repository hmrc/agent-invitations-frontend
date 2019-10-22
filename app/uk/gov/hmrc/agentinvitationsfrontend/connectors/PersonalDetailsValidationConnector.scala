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

import com.codahale.metrics.MetricRegistry
import com.kenshoo.play.metrics.Metrics
import javax.inject.{Inject, Named, Singleton}
import play.api.i18n.Messages
import play.api.mvc.Results._
import play.api.mvc.{Request, Result}
import play.api.{Configuration, Environment, Logger}
import uk.gov.hmrc.agent.kenshoo.monitoring.HttpAPIMonitor
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients.cannot_confirm_identity
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, HttpGet, HttpResponse, NotFoundException}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PersonalDetailsValidationConnector @Inject()(
  @Named("personal-details-validation-baseUrl") baseUrl: URL,
  metrics: Metrics,
  http: HttpGet,
  env: Environment,
  config: Configuration)
    extends HttpAPIMonitor {

  override val kenshooRegistry: MetricRegistry = metrics.defaultRegistry

  def getPdvResult(
    validationId: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[PdvError, Nino]] =
    monitor("ConsumedAPI-Client-Get-PDVResult-GET") {
      val url = s"${baseUrl.toString}/$validationId"
      http
        .GET[HttpResponse](url)
        .map { response =>
          response.status match {
            case 200 =>
              val result = (response.json \ "validationStatus").as[String]
              if (result == "success") {
                (response.json \ "personalDetails" \ "nino").asOpt[Nino].toRight[PdvError](PdvValidationNoNino)
              } else {
                Left(PdvValidationFailure)
              }
          }
        }
        .recover {
          case e: NotFoundException => {
            Logger.warn(s"personal details validation did not recognise the validation is $validationId $e")
            Left(PdvValidationNotFound)
          }
        }
    }
}

sealed trait PdvError {
  def Result[A](validationId: String)(
    implicit request: Request[A],
    messages: Messages,
    configuration: Configuration,
    externalUrls: ExternalUrls): Result = this match {
    case PdvValidationNotFound =>
      InternalServerError(s"failed to get PDV result: data for validationId $validationId not found")
    case PdvValidationNoNino  => InternalServerError(s"failed to get PDV result: No NINO in response for $validationId")
    case PdvValidationFailure => Forbidden(cannot_confirm_identity())
  }
}

case object PdvValidationNotFound extends PdvError
case object PdvValidationNoNino extends PdvError
case object PdvValidationFailure extends PdvError
