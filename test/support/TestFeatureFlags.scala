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

import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentmtdidentifiers.model.Service

case class TestFeatureFlags(enabled: Set[Service] = Service.supportedServices.toSet) extends FeatureFlags {
  private def affectedServiceKeys(service: Service): Set[Service] = service match {
    case Service.MtdIt                   => Set(Service.MtdIt)
    case Service.PersonalIncomeRecord    => Set(Service.PersonalIncomeRecord)
    case Service.Vat                     => Set(Service.Vat)
    case Service.Trust | Service.TrustNT => Set(Service.Trust, Service.TrustNT)
    case Service.CapitalGains            => Set(Service.CapitalGains)
    case Service.Ppt                     => Set(Service.Ppt)
    case Service.Cbc | Service.CbcNonUk  => Set(Service.Cbc, Service.CbcNonUk)
    case Service.Pillar2                 => Set(Service.Pillar2)
  }
  def enable(services: Service*): TestFeatureFlags =
    this.copy(enabled = services.foldLeft(this.enabled) { case (a, e) => a.union(affectedServiceKeys(e)) })
  def disable(services: Service*): TestFeatureFlags =
    this.copy(enabled = services.foldLeft(this.enabled) { case (a, d) => a.diff(affectedServiceKeys(d)) })

  override def isServiceEnabled(service: Service): Boolean = enabled.contains(service)
}

object TestFeatureFlags {
  def allDisabled: TestFeatureFlags = TestFeatureFlags(Set.empty)
  def allEnabled: TestFeatureFlags = TestFeatureFlags(Service.supportedServices.toSet)
}
