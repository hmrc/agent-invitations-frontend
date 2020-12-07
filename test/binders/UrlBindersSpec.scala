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

package binders

import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import uk.gov.hmrc.agentinvitationsfrontend.binders.{ErrorConstants, UrlBinders}
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus.AllStatuses
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.play.test.UnitSpec

class UrlBindersSpec extends UnitSpec with GuiceOneServerPerSuite {

  private val invitationIdError = Left(ErrorConstants.InvitationIdNotFound)
  private val statusError = Left(ErrorConstants.StatusError)

  "InvitationId binder" should {
    "bind an invitation id from a valid string" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "ABERULMHCKKW3") shouldBe Right(InvitationId("ABERULMHCKKW3"))
    }

    "return error if the input has an invalid format" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "ABERULMHCBBBBBKKW3") shouldBe invitationIdError
      UrlBinders.invitationIdBinder.bind("invitationId", "123") shouldBe invitationIdError
    }

    "return error if the input has an invalid CRC" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "ABERULMHCKKXX") shouldBe invitationIdError
    }

    "return error if the input has an invalid prefix" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "XG3HFG43HW2PF") shouldBe invitationIdError
    }
  }

  "FilterFormStatus binder" should {
    "bind a filter status from a valid string" in {
      UrlBinders.filterFormStatusBinder.bind("status", Map("status" -> List("AllStatuses"))) shouldBe Some(Right(AllStatuses))
    }

    "unbind a valid status to a String" in {
      UrlBinders.filterFormStatusBinder
        .unbind("status", FilterFormStatus.AcceptedByClient) shouldBe "status=AcceptedByClient"
    }

    "return an error if the input is not a valid status" in {
      UrlBinders.filterFormStatusBinder.bind("status", Map("status" -> List("NotAStatus"))) shouldBe Some(statusError)
    }
  }
}
