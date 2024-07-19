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

import java.net.URL
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Service}

import java.time.{LocalDate, LocalDateTime}

case class StoredInvitation(
  arn: Arn,
  clientType: Option[String],
  service: Service,
  clientId: String,
  clientIdType: String,
  suppliedClientId: String,
  suppliedClientIdType: String,
  detailsForEmail: Option[DetailsForEmail],
  status: String,
  created: LocalDateTime,
  lastUpdated: LocalDateTime,
  expiryDate: LocalDate,
  invitationId: String,
  isRelationshipEnded: Boolean,
  relationshipEndedBy: Option[String] = None,
  selfUrl: URL
) {
  val altItsa: Option[Boolean] = if (service == Service.MtdIt) Some(clientId == suppliedClientId) else None
}

object StoredInvitation {

  def apply(
    arn: Arn,
    clientType: Option[String],
    service: Service,
    clientId: String,
    detailsForEmail: Option[DetailsForEmail],
    status: String,
    created: LocalDateTime,
    lastUpdated: LocalDateTime,
    expiryDate: LocalDate,
    invitationId: String,
    isRelationshipEnded: Boolean,
    relationshipEndedBy: Option[String],
    selfUrl: URL
  ): StoredInvitation =
    StoredInvitation(
      arn,
      clientType,
      service,
      clientId,
      service.supportedSuppliedClientIdType.id,
      clientId,
      service.supportedSuppliedClientIdType.id,
      detailsForEmail,
      status,
      created,
      lastUpdated,
      expiryDate,
      invitationId,
      isRelationshipEnded,
      relationshipEndedBy,
      selfUrl
    )

}

case class DetailsForEmail(agencyEmail: String, agencyName: String, clientName: String)
