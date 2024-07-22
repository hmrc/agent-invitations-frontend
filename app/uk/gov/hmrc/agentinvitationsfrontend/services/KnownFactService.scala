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

import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class KnownFactService @Inject() (
  val acaConnector: AgentClientAuthorisationConnector,
  val citizenDetailsConnector: CitizenDetailsConnector,
  val relationshipsConnector: RelationshipsConnector
) extends GetClientName with FrontendHeaderCarrierProvider with Logging {

  def checkKnownFact(clientIdSet: ClientIdSet)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[KnownFactResult] =
    (clientIdSet match {
      case VatClient(vrnStr, regDateStr) =>
        acaConnector.checkVatRegisteredClient(vrnStr, LocalDate.parse(regDateStr))
      case PptClient(pptRef, regDateStr) =>
        acaConnector.checkKnownFactPPT(PptClient(pptRef, regDateStr))
      case CbcClient(cbcId, email) =>
        acaConnector.checkCbcEmailKnownFact(cbcId, email)
      case ItsaClient(nino, postcode) =>
        acaConnector.checkPostcodeForClient(nino, postcode)
      case IrvClient(nino, dobStr) =>
        acaConnector.checkCitizenRecord(nino, LocalDate.parse(dobStr))
      case Pillar2Client(plrId, regDateStr) =>
        acaConnector.checkPillar2KnownFact(plrId, LocalDate.parse(regDateStr))
      case CgtClient(cgtRef, Left(countryCode)) =>
        acaConnector.getCgtSubscription(cgtRef).map {
          case Some(subscription) if subscription.countryCode.contains(countryCode.value) => Pass
          case _                                                                          => Fail(NotMatched)
        }
      case CgtClient(cgtRef, Right(postcode)) =>
        acaConnector.getCgtSubscription(cgtRef).map {
          case Some(subscription) if subscription.postCode.contains(postcode.value) => Pass
          case _                                                                    => Fail(NotMatched)
        }
      case TrustClient(_) => Future.successful(Pass) // no known fact check for trusts
    }).map {
      // if an unexpected http response is received, throw an exception to maintain legacy behaviour - this case should ideally be handled downstream, though
      case Fail(HttpStatus(x)) => throw UpstreamErrorResponse("Known fact check: unexpected status " + x, x)
      case anyOther            => anyOther
    }
}
