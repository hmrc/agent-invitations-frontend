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

import support.{TestFeatureFlags, TestIdentifiers, UnitSpec}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Service
import uk.gov.hmrc.agentmtdidentifiers.model.Service._

class InvitationsBasketSpec extends UnitSpec {

  "InvitationsBasket" should {
    val services: Set[Service] = Set(Ppt, Trust, Cbc, CapitalGains)
    val emptyBasket = InvitationsBasket(ClientType.Trust, services, basket = Set.empty, featureFlags = TestFeatureFlags.allEnabled)
    def basketWith(services: Service*) =
      emptyBasket.copy(
        basket =
          services.map(svc => AuthorisationRequest("name", Invitation(Some(ClientType.Business), svc, TestIdentifiers.anIdentifierFor(svc)))).toSet
      )

    "calculate services available to select based on what is in the basket" in {
      // return all services when nothing is selected
      emptyBasket.availableServices shouldBe services
      // remove services that are already selected
      basketWith(Ppt).availableServices shouldBe Set(Trust, Cbc, CapitalGains)
      // remove an aliased service when any of its aliases are selected (e.g. Trust)
      basketWith(Trust).availableServices shouldBe Set(Ppt, Cbc, CapitalGains)
      basketWith(TrustNT).availableServices shouldBe Set(Ppt, Cbc, CapitalGains)
      // remove an aliased service when any of its aliases are selected (e.g. Cbc)
      basketWith(Cbc).availableServices shouldBe Set(Ppt, Trust, CapitalGains)
      basketWith(CbcNonUk).availableServices shouldBe Set(Ppt, Trust, CapitalGains)
      // remove services with multiple ones are selected
      basketWith(Ppt, CapitalGains).availableServices shouldBe Set(Trust, Cbc)
      // return nothing when all services are selected
      basketWith(services.toSeq: _*).availableServices shouldBe Set.empty
    }
  }
}
