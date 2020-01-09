/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.wiring

import akka.stream.Materializer
import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.http.HttpFilters
import play.api.http.HttpVerbs.POST
import play.api.mvc.{EssentialFilter, Filter, RequestHeader, Result}
import uk.gov.hmrc.play.bootstrap.filters.FrontendFilters

import scala.concurrent.Future

@Singleton
class ExtendedFrontendFilters @Inject()(
  defaultFilters: FrontendFilters,
  configuration: Configuration,
  materializer: Materializer)
    extends HttpFilters {

  override def filters: Seq[EssentialFilter] = CSRFExceptionsFilter +: defaultFilters.filters

  object CSRFExceptionsFilter extends Filter {

    lazy val whitelist: Set[String] = configuration
      .get[Option[Seq[String]]]("csrfexceptions.whitelist")
      .getOrElse(Seq.empty)
      .toSet

    override implicit def mat: Materializer = materializer

    def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {

      def filteredHeaders(rh: RequestHeader): RequestHeader =
        if (rh.method == POST && whitelist.contains(rh.path))
          rh.withHeaders(rh.headers.add("Csrf-Token" -> "nocheck"))
        else rh

      f(filteredHeaders(rh))
    }

  }

}
