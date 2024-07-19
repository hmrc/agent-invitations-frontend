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

import play.api.libs.json.{JsResultException, Json}
import support.UnitSpec
import uk.gov.hmrc.agentinvitationsfrontend.models.PptSubscription

import java.time.LocalDate

class PptSubscriptionSpec extends UnitSpec {

  "PptSubscription" should {

    "create a PptSubscription with valid Json" in {

      val json =
        s"""{
           | "legalEntityDetails": {
           |    "dateOfApplication": "2021-10-12",
           |    "customerDetails": {
           |        "customerType": "Individual",
           |        "individualDetails": {
           |          "firstName": "John",
           |          "lastName": "Doe"
           |       }
           |    }
           | },
           | "changeOfCircumstanceDetails": {
           |    "deregistrationDetails": {
           |        "deregistrationDate": "2021-11-12"
           |    }
           | }
           |}""".stripMargin

      Json.parse(json).as[PptSubscription] shouldBe PptSubscription("John Doe", LocalDate.parse("2021-10-12"), Some(LocalDate.parse("2021-11-12")))
    }

    "create a PptSubscription with valid Json deregistration date missing" in {

      val json =
        s"""{
           | "legalEntityDetails": {
           |    "dateOfApplication": "2021-10-12",
           |    "customerDetails": {
           |        "customerType": "Individual",
           |        "individualDetails": {
           |          "firstName": "John",
           |          "lastName": "Doe"
           |       }
           |    }
           | },
           | "changeOfCircumstanceDetails": {
           | }
           |}""".stripMargin

      Json.parse(json).as[PptSubscription] shouldBe PptSubscription("John Doe", LocalDate.parse("2021-10-12"), None)
    }

    "create a PptSubscription with Organisation type" in {

      val json =
        s"""{
           | "legalEntityDetails": {
           |    "dateOfApplication": "2021-10-12",
           |    "customerDetails": {
           |        "customerType": "Organisation",
           |        "organisationDetails": {
           |          "organisationName": "My PPT organisation"
           |       }
           |    }
           | },
           | "changeOfCircumstanceDetails": {
           |    "deregistrationDetails": {
           |        "deregistrationDate": "2021-11-12"
           |    }
           | }
           |}""".stripMargin

      Json.parse(json).as[PptSubscription] shouldBe PptSubscription(
        "My PPT organisation",
        LocalDate.parse("2021-10-12"),
        Some(LocalDate.parse("2021-11-12"))
      )
    }

    "raise JsError when invalid field" in {

      intercept[JsResultException] {
        val json =
          s"""{
             | "legalEntityDetails": {
             |    "dateOfSubmission": "2021-10-12",
             |    "customerDetails": {
             |        "customerType": "Organisation",
             |        "organisationDetails": {
             |          "organisationName": "My PPT organisation"
             |       }
             |    }
             | },
             | "changeOfCircumstanceDetails": {
             |    "deregistrationDetails": {
             |        "deregistrationDate": "2021-11-12"
             |    }
             | }
             |}""".stripMargin

        Json.parse(json).as[PptSubscription]
      }

    }
  }

}
