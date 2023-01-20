/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.json.{JsString, Json, OFormat}
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import support.UnitSpec

class SelectClientTypeSpec extends UnitSpec {

  case class ClientTypeTest(clientType: ClientType)
  object ClientTypeTest {
    implicit val formats: OFormat[ClientTypeTest] = Json.format[ClientTypeTest]
  }

  "ClientType" should {
    "serialize to json string" in {
      Json.toJson(ClientType.Personal: ClientType) shouldBe JsString("personal")
      Json.toJson(ClientType.Business: ClientType) shouldBe JsString("business")
    }

    "deserialize from json string" in {
      Json.parse("""{"clientType":"personal"}""").as[ClientTypeTest].clientType shouldBe ClientType.Personal
      Json.parse("""{"clientType":"business"}""").as[ClientTypeTest].clientType shouldBe ClientType.Business
    }

  }
}
