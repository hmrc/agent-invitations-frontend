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

package support

import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}

object TestIdentifiers {
  val vrn = Vrn("123456")
  val nino = Nino("AB123456A")
  val utr = Utr("1977030537")
  val urn = Urn("XXTRUST10010010")
  val cgtRef = CgtRef("XMCGTP123456789")
  val pptRef = PptRef("XAPPT0000012345")
  val cbcId = CbcId("XACBC0000011111")
  val plrId = PlrId("XAPLR2222222222")

  def anIdentifierFor(service: Service): TaxIdentifier = service match {
    case Service.MtdIt                  => nino
    case Service.PersonalIncomeRecord   => nino
    case Service.Vat                    => vrn
    case Service.Trust                  => utr
    case Service.TrustNT                => urn
    case Service.CapitalGains           => cgtRef
    case Service.Ppt                    => pptRef
    case Service.Cbc | Service.CbcNonUk => cbcId
    case Service.Pillar2                => plrId
  }
}
