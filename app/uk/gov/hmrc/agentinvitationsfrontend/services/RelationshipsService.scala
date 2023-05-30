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
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Service}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsService @Inject()(relationshipsConnector: RelationshipsConnector, pirRelationshipConnector: PirRelationshipConnector) {

  def hasActiveRelationshipFor(arn: Arn, clientId: String, service: Service)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Boolean] =
    service match {
      case Service.PersonalIncomeRecord => pirRelationshipConnector.getPirRelationshipForAgent(arn, Nino(clientId)).map(_.nonEmpty)
      case _                            => relationshipsConnector.checkRelationship(service, arn, service.supportedSuppliedClientIdType.createUnderlying(clientId))
    }

  def deleteRelationshipForService(service: Service, arn: Arn, clientId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    service match {
      case Service.PersonalIncomeRecord => pirRelationshipConnector.deleteRelationship(arn, service, clientId)
      case _                            => relationshipsConnector.deleteRelationshipForService(service, arn, service.supportedSuppliedClientIdType.createUnderlying(clientId))
    }
}
