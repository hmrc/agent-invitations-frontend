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

package binders

import org.scalatestplus.play.OneAppPerSuite
import uk.gov.hmrc.agentinvitationsfrontend.binders.{ErrorConstants, UrlBinders}
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.play.test.UnitSpec

class UrlBindersSpec extends UnitSpec with OneAppPerSuite {

  private val error = Left(ErrorConstants.InvitationIdNotFound)

  "InvitationId binder" should {
    "bind an invitation id from a valid string" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "ABERULMHCKKW3") shouldBe Right(InvitationId("ABERULMHCKKW3"))
    }

    "return error if the input has an invalid format" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "ABERULMHCBBBBBKKW3") shouldBe error
      UrlBinders.invitationIdBinder.bind("invitationId", "123") shouldBe error
    }

    "return error if the input has an invalid CRC" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "ABERULMHCKKXX") shouldBe error
    }

    "return error if the input has an invalid prefix" in {
      UrlBinders.invitationIdBinder.bind("invitationId", "XG3HFG43HW2PF") shouldBe error
    }
  }

}
