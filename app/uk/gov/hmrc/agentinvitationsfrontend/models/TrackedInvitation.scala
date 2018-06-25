/*
 * Copyright 2018 HM Revenue & Customs
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

import org.joda.time.{DateTime, LocalDate}

case class TrackedInvitation(
  service: String,
  clientId: String,
  clientIdType: String,
  clientName: Option[String],
  status: String,
  lastUpdated: DateTime,
  expiryDate: LocalDate
) extends ServiceAndClient

object TrackedInvitation {

  val preferredIdTypes = Set("ni", "vrn")

  def fromStored(i: StoredInvitation)(implicit now: LocalDate): TrackedInvitation = {

    val (clientId, clientIdType) =
      if (preferredIdTypes.contains(i.clientIdType)
          || i.suppliedClientIdType.isEmpty
          || i.suppliedClientId.isEmpty) (i.clientId, i.clientIdType)
      else (i.suppliedClientId, i.suppliedClientIdType)

    val status =
      if (i.status == "Pending" && (now.isAfter(i.expiryDate) || now.isEqual(i.expiryDate))) "Expired"
      else i.status

    TrackedInvitation(i.service, clientId, clientIdType, None, status, i.lastUpdated, i.expiryDate)
  }
}
