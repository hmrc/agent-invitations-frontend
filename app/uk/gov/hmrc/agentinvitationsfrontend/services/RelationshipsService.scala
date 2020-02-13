/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.services
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsService @Inject()(
  relationshipsConnector: RelationshipsConnector,
  pirRelationshipConnector: PirRelationshipConnector) {

  def hasActiveRelationshipFor(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Boolean] =
    service match {
      case HMRCMTDIT  => relationshipsConnector.checkItsaRelationship(arn, Nino(clientId))
      case HMRCMTDVAT => relationshipsConnector.checkVatRelationship(arn, Vrn(clientId))
      case HMRCPIR    => pirRelationshipConnector.getPirRelationshipForAgent(arn, Nino(clientId)).map(_.nonEmpty)
      case TRUST      => relationshipsConnector.checkTrustRelationship(arn, Utr(clientId))
      case HMRCCGTPD  => relationshipsConnector.checkCgtRelationship(arn, CgtRef(clientId))
    }

  def deleteRelationshipForService(service: String, arn: Arn, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    service match {
      case HMRCMTDIT  => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case HMRCPIR    => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case HMRCMTDVAT => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case TRUST      => relationshipsConnector.deleteRelationshipTrust(arn, Utr(clientId))
      case HMRCCGTPD  => relationshipsConnector.deleteRelationshipCgt(arn, CgtRef(clientId))
      case e          => throw new Error(s"Unsupported service for deleting relationship: $e")
    }
}
