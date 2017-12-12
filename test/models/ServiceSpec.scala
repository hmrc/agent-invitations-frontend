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

package models

import uk.gov.hmrc.agentinvitationsfrontend.controllers.Services._
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId
import uk.gov.hmrc.play.test.UnitSpec

class ServiceSpec extends UnitSpec {

  "Services" should {
    "return ITSA if given prefix of invitationId is A" in {
      determineServices(InvitationId("A6WTS5241C99B")).value shouldBe "itsa"
    }

    "return AFI if given prefix of invitationId is B" in {
      determineServices(InvitationId("BBERULMHCKK")).value shouldBe "afi"
    }

    "return Exception when given invalid invitationId" in {
      determineServices(InvitationId("CBERULMHCKK")).value shouldBe "Not Supported"
    }

  }
}
