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

package uk.gov.hmrc.agentinvitationsfrontend.models

import play.api.libs.json._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}

/**
  * A single-string and human-friendly Json format for the TaxIdentifiers we deal with in Agents.
  */
object TaxIdFormat {
  /* TODO [Service onboarding]
     Here we have yet another "client id type <-> tag" mapping that is inconsistent with other places.
     Consider changing these tags to be the same as the ones used elsewhere.
     (But we must ensure there is no persistent data stored under this format - other than temp session data)
   */
  implicit val taxIdFormat = new Format[TaxIdentifier] {
    override def writes(o: TaxIdentifier): JsValue = o match {
      case x: Nino   => JsString(s"Nino|${x.value}")
      case x: Vrn    => JsString(s"Vrn|${x.value}")
      case x: Utr    => JsString(s"Utr|${x.value}")
      case x: Urn    => JsString(s"Urn|${x.value}")
      case x: CgtRef => JsString(s"CgtRef|${x.value}")
      case x: PptRef => JsString(s"PptRef|${x.value}")
      case x: CbcId  => JsString(s"CbcId|${x.value}")
      case x: PlrId  => JsString(s"PlrId|${x.value}")
      case x         => throw new IllegalArgumentException(s"Unsupported tax identifier: $x")
    }

    override def reads(json: JsValue): JsResult[TaxIdentifier] = json match {
      case JsString(str) if str.matches("""\w{3,6}\|[\w\d]+""") =>
        val idType :: id :: Nil = str.split("""\|""").toList
        idType match {
          case "Nino"   => JsSuccess(Nino(id))
          case "Vrn"    => JsSuccess(Vrn(id))
          case "Utr"    => JsSuccess(Utr(id))
          case "Urn"    => JsSuccess(Urn(id))
          case "CgtRef" => JsSuccess(CgtRef(id))
          case "PptRef" => JsSuccess(PptRef(id))
          case "CbcId"  => JsSuccess(CbcId(id))
          case "PlrId"  => JsSuccess(PlrId(id))
          case x        => JsError(s"Invalid tax identifier type: $x")
        }
      case _ => JsError("Invalid tax identifier JSON")
    }
  }
}
