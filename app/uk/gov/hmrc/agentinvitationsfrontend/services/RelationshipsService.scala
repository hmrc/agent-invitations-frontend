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

package uk.gov.hmrc.agentinvitationsfrontend.services

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{AgentServicesAccountConnector, CitizenDetailsConnector, DesConnector, RelationshipsConnector}
import uk.gov.hmrc.agentinvitationsfrontend.models.{InactiveClient, ItsaTrackRelationship, TrackRelationship}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RelationshipsService @Inject()(
  relationshipsConnector: RelationshipsConnector,
  agentServicesAccountConnector: AgentServicesAccountConnector,
  citizenDetailsConnector: CitizenDetailsConnector,
  desConnector: DesConnector) {

  def getInvalidClients(nino: Option[Nino], vrn: Option[Vrn])(
    implicit c: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[InactiveClient]] = {
    val itsaRelationships: Future[Seq[ItsaTrackRelationship]] = relationshipsConnector.getInactiveItsaRelationships
    val vatRelationships = relationshipsConnector.getInactiveVatRelationships

//    for {
////      inactiveRelationships <- Future.sequence(Seq(itsaRelationships, vatRelationships)).map(_.flatten)
//      ir           <- itsaRelationships
//      inactiveItsa <- formatItsa(ir)
//
//    } yield inactiveItsa

    for {
      inactiveRelationships <- Future.sequence(Seq(itsaRelationships, vatRelationships)).map(_.flatten)
      inactiveClients <- inactiveRelationships.map(inactiveRelationship => {
                                                       if (inactiveRelationship.serviceName == "HMRC-MTD-IT") {
                                                         for {
                                                           ninoForMtdItId <- desConnector.getNinoFor(
                                                                              MtdItId(inactiveRelationship.clientId))
                                                           tradeName <- getTradingName(Some(ninoForMtdItId))
                                                         } yield
                                                           InactiveClient(
                                                             "HMRC-MTD-IT",
                                                             tradeName.getOrElse(""),
                                                             inactiveRelationship.dateTo)
                                                       } else if (inactiveRelationship.serviceName == "HMRC-MTD-VAT") {
                                                         for {
                                                           vatName <- getVatName(
                                                                       Some(Vrn(inactiveRelationship.clientId)))
                                                                       .map(_.getOrElse(""))
                                                         } yield
                                                           InactiveClient(
                                                             "HMRC-MTD-VAT",
                                                             vatName,
                                                             inactiveRelationship.dateTo)
                                                       } else Future successful Seq(InactiveClient("", "", None))
                                                     })
    } yield inactiveClients
  }

//  private def formatItsa(itsaRels: Seq[ItsaTrackRelationship]): Future[Seq[InactiveClient]] =
//    for {
//      itsaRelation <- Future.traverse(itsaRels)
//      nino         <- desConnector.getNinoFor(MtdItId(itsaRelation))
//      name         <- getTradingName(Some(nino))
//
//    } yield InactiveClient(itsaRelation, name.getOrElse(""), itsaRelation.dateTo)

  def deleteITSAInvitation(arn: Arn, nino: Nino)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    relationshipsConnector.deleteItsaRelationship(arn, nino)

  def deleteVATInvitation(arn: Arn, vrn: Vrn)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[Boolean]] =
    relationshipsConnector.deleteVatRelationship(arn, vrn)

  def relationships(identifierOpt: Option[TaxIdentifier])(f: TaxIdentifier => Future[Seq[TrackRelationship]]) =
    identifierOpt match {
      case Some(identifier) => f(identifier)
      case None             => Future.successful(Seq.empty)
    }

  def names(identifierOpt: Option[TaxIdentifier])(f: TaxIdentifier => Future[Seq[String]]) = identifierOpt match {
    case Some(identifier) => f(identifier)
    case None             => Future.successful(Seq.empty)
  }

  def getTradingName(
    clientIdentifier: Option[Nino])(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier match {
      case Some(s) => agentServicesAccountConnector.getTradingName(s)
      case None    => Future successful None
    }

  def getCitizenName(
    clientIdentifier: Option[Nino])(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier match {
      case Some(s) => citizenDetailsConnector.getCitizenDetails(s).map(citizen => citizen.name)
      case None    => Future successful None
    }

  def getVatName(
    clientIdentifier: Option[Vrn])(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier
      .map { vrn =>
        agentServicesAccountConnector.getCustomerDetails(vrn).map { customerDetails =>
          customerDetails.tradingName
            .orElse(customerDetails.organisationName)
            .orElse(customerDetails.individual.map(_.name))
        }
      }
      .getOrElse(Future.successful(None))

//  def getNames(
//    trackRelationships: Seq[TrackRelationship])(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Seq[Option[String]]] =
//    trackRelationships.map(trackRelationship => getTradingName(Some(Nino(trackRelationship.clientId)))
//    )
}
