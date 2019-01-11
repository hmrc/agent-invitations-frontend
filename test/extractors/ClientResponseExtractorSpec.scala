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

package extractors

import uk.gov.hmrc.agentinvitationsfrontend.controllers.ClientsInvitationController.IsServiceMessageKeyValid
import uk.gov.hmrc.agentinvitationsfrontend.models.{InvalidService, ValidService}
import uk.gov.hmrc.play.test.UnitSpec

class ClientResponseExtractorSpec extends UnitSpec {

  "The IsServiceMessageKeyValid extractor" should {
    "Return a message key when service is valid and message key is non empty" in {
      val args = ValidService("", "", "", "", "itsa")
      IsServiceMessageKeyValid.unapply(args) shouldBe Some("itsa")
    }

    "Return None when service is valid and message key is empty" in {
      val args = ValidService("", "", "", "", "")
      IsServiceMessageKeyValid.unapply(args) shouldBe None
    }

    "Return None when service is invalid" in {
      val args = InvalidService
      IsServiceMessageKeyValid.unapply(args) shouldBe None
    }
  }
}
