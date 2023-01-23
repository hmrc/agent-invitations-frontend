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

package uk.gov.hmrc.agentinvitationsfrontend.services
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, PptRef, Service, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsService @Inject()(relationshipsConnector: RelationshipsConnector, pirRelationshipConnector: PirRelationshipConnector) {

  def hasActiveRelationshipFor(arn: Arn, clientId: String, service: Service)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    service match {
      case Service.MtdIt                => relationshipsConnector.checkItsaRelationship(arn, Nino(clientId))
      case Service.Vat                  => relationshipsConnector.checkVatRelationship(arn, Vrn(clientId))
      case Service.PersonalIncomeRecord => pirRelationshipConnector.getPirRelationshipForAgent(arn, Nino(clientId)).map(_.nonEmpty)
      case Service.Trust                => relationshipsConnector.checkTrustRelationship(arn, Utr(clientId))
      case Service.TrustNT              => relationshipsConnector.checkTrustNTRelationship(arn, Urn(clientId))
      case Service.CapitalGains         => relationshipsConnector.checkCgtRelationship(arn, CgtRef(clientId))
      case Service.Ppt                  => relationshipsConnector.checkPptRelationship(arn, PptRef(clientId))
    }

  def deleteRelationshipForService(service: Service, arn: Arn, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    service match {
      case Service.MtdIt                => relationshipsConnector.deleteRelationshipItsa(arn, Nino(clientId))
      case Service.PersonalIncomeRecord => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case Service.Vat                  => relationshipsConnector.deleteRelationshipVat(arn, Vrn(clientId))
      case Service.Trust                => relationshipsConnector.deleteRelationshipTrust(arn, Utr(clientId))
      case Service.TrustNT              => relationshipsConnector.deleteRelationshipTrustNT(arn, Urn(clientId))
      case Service.CapitalGains         => relationshipsConnector.deleteRelationshipCgt(arn, CgtRef(clientId))
      case Service.Ppt                  => relationshipsConnector.deleteRelationshipPpt(arn, PptRef(clientId))
      case e                            => throw new Error(s"Unsupported service for deleting relationship: $e")
    }
}
