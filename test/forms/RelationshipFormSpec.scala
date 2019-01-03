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

package forms

import play.api.libs.json.Json
import uk.gov.hmrc.agentinvitationsfrontend.controllers.testing.TestEndpointsController._
import uk.gov.hmrc.agentmtdidentifiers.model.Arn
import uk.gov.hmrc.play.test.UnitSpec

class RelationshipFormSpec extends UnitSpec {

  val arn = Arn("TARN0000001")
  val clientId = "AA123456A"
  val afiService = "PERSONAL-INCOME-RECORD"

  "ConfirmInvite form" should {
    "return no error with valid input" in {
      val result =
        testRelationshipForm.bind(Json.obj("arn" -> arn.value, "service" -> afiService, "clientId" -> clientId))

      result.errors.isEmpty shouldBe true
    }

    "return no errors when unbinding the form" in {
      val unboundForm = testRelationshipForm.mapping.unbind(Relationship(arn, afiService, clientId))
      unboundForm("arn") shouldBe arn.value
      unboundForm("service") shouldBe afiService
      unboundForm("clientId") shouldBe clientId
    }
  }

}
