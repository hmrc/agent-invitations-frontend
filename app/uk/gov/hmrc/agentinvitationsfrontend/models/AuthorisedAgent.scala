/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Service}

case class AuthorisedAgent(arn: Arn) {

  val personalServices: Set[Service] = Set(Service.PersonalIncomeRecord, Service.MtdIt, Service.Vat, Service.CapitalGains, Service.Ppt)

  val businessServices: Set[Service] = Set(Service.Vat, Service.Ppt)

  val trustServices: Set[Service] = Set(Service.Trust, Service.CapitalGains, Service.Ppt)
}
