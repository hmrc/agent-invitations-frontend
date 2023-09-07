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

import uk.gov.hmrc.agentmtdidentifiers.model.Service

case class ConfirmedTerms(
  itsaConsent: Boolean,
  afiConsent: Boolean,
  vatConsent: Boolean,
  trustConsent: Boolean,
  cgtConsent: Boolean,
  trustNTConsent: Boolean,
  pptConsent: Boolean,
  cbcConsent: Boolean // a client will never have both CBC variants
) {

  def get(service: Service): Boolean = service match {
    case Service.MtdIt                => this.itsaConsent
    case Service.PersonalIncomeRecord => this.afiConsent
    case Service.Vat                  => this.vatConsent
    case Service.Trust                => this.trustConsent
    case Service.TrustNT              => this.trustNTConsent
    case Service.CapitalGains         => this.cgtConsent
    case Service.Ppt                  => this.pptConsent
    case Service.Cbc                  => this.cbcConsent
    case Service.CbcNonUk             => this.cbcConsent
  }
}

object ConfirmedTerms {
  def forServices(services: Service*): ConfirmedTerms = ConfirmedTerms(
    services.contains(Service.MtdIt),
    services.contains(Service.PersonalIncomeRecord),
    services.contains(Service.Vat),
    services.contains(Service.Trust),
    services.contains(Service.CapitalGains),
    services.contains(Service.TrustNT),
    services.contains(Service.Ppt),
    services.contains(Service.Cbc) || services.contains(Service.CbcNonUk),
  )
}
