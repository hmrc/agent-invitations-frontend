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

import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.{urnPattern, utrPattern}
import uk.gov.hmrc.agentmtdidentifiers.model.{CbcId, CgtRef, PlrId, PptRef, TrustTaxIdentifier, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino

/** A set of data that is sufficient to identify and verify a client by checking the associated known fact.
  */
sealed trait ClientIdSet

case class CbcClient(cbcId: CbcId, email: String) extends ClientIdSet

case class CgtClient(cgtRef: CgtRef, knownFact: Either[CountryCode, Postcode]) extends ClientIdSet

case class IrvClient(nino: Nino, dob: String) extends ClientIdSet

case class ItsaClient(nino: Nino, postcode: String) extends ClientIdSet

case class PptClient(pptRef: PptRef, registrationDate: String) extends ClientIdSet

case class VatClient(vrn: Vrn, registrationDate: String) extends ClientIdSet

case class TrustClient(taxId: TrustTaxIdentifier) extends ClientIdSet

object TrustClient {
  def apply(taxId: String): TrustClient = taxId match {
    case x if x.matches(utrPattern) => TrustClient(Utr(x))
    case x if x.matches(urnPattern) => TrustClient(Urn(x))
  }
}

case class Pillar2Client(plrId: PlrId, registrationDate: String) extends ClientIdSet
