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

package uk.gov.hmrc.agentinvitationsfrontend.models

import java.net.URL

import org.joda.time.{DateTime, LocalDate}
import uk.gov.hmrc.agentmtdidentifiers.model.Arn

case class StoredInvitation(
  arn: Arn,
  clientType: Option[String],
  service: String,
  clientId: String,
  clientIdType: String,
  suppliedClientId: String,
  suppliedClientIdType: String,
  status: String,
  created: DateTime,
  lastUpdated: DateTime,
  expiryDate: LocalDate,
  invitationId: String,
  selfUrl: URL)
    extends ServiceAndClient

object StoredInvitation {

  val clientIdTypeByService: String => String = {
    case "HMRC-MTD-IT"            => "ni"
    case "HMRC-MTD-VAT"           => "vrn"
    case "PERSONAL-INCOME-RECORD" => "ni"
    case "HMRC-TERS-ORG"          => "utr"
    case _                        => throw new IllegalArgumentException()
  }

  def apply(
    arn: Arn,
    clientType: Option[String],
    service: String,
    clientId: String,
    status: String,
    created: DateTime,
    lastUpdated: DateTime,
    expiryDate: LocalDate,
    invitationId: String,
    selfUrl: URL): StoredInvitation =
    StoredInvitation(
      arn,
      clientType,
      service,
      clientId,
      clientIdTypeByService(service),
      clientId,
      clientIdTypeByService(service),
      status,
      created,
      lastUpdated,
      expiryDate,
      invitationId,
      selfUrl
    )

}
