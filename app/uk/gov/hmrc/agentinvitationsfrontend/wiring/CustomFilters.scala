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

package uk.gov.hmrc.agentinvitationsfrontend.wiring

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

import akka.util.ByteString
import javax.inject.Inject
import play.api.libs.streams.Accumulator
import play.api.mvc.Results._
import play.api.mvc._
import play.api.routing.Router
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes

import scala.concurrent.ExecutionContext

//class CustomFilters @Inject()(
//  override val mat: Materializer
//) extends Filter {
//
//  def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] =
//    f(redirectWhenPost(rh))
//
//  private def redirectWhenPost(rh: RequestHeader) =
//    if (rh.flash.data.contains("redirectFlash")) {
//      println(s"Flash is here: $rh")
//      Redirect(routes.AgentInvitationJourneyController.showIdentifyClient())
//    } else {
//      rh
//    }
//
//}

class CustomFilters @Inject()(router: Router)(implicit ec: ExecutionContext) extends EssentialFilter {

  override def apply(next: EssentialAction): EssentialAction = new EssentialAction {
    override def apply(request: RequestHeader): Accumulator[ByteString, Result] = {
      val accumulator: Accumulator[ByteString, Result] = next(request)

      accumulator.map { result =>
        val postUrlsWithoutTwin: Map[String, Call] =
          Map(
            "/invitations/agents/identify-itsa-client" -> routes.AgentInvitationJourneyController.showIdentifyClient(),
            "/invitations/agents/identify-irv-client"  -> routes.AgentInvitationJourneyController.showIdentifyClient(),
            "/invitations/agents/identify-vat-client"  -> routes.AgentInvitationJourneyController.showIdentifyClient()
          )

        if (postUrlsWithoutTwin.keySet.contains(request.uri) && request.method == "GET") {
          println(s"${router.documentation}")
          val correspondingGet: Call =
            postUrlsWithoutTwin.getOrElse(request.uri, throw new Exception("no corresponding GET"))
          Redirect(correspondingGet)
        } else result
      }
    }
  }
}
