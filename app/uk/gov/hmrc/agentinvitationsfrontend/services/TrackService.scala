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
import org.joda.time.{DateTimeZone, LocalDate}
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TrackService @Inject()(
  invitationsConnector: InvitationsConnector,
  relationshipsConnector: RelationshipsConnector,
  pirRelationshipConnector: PirRelationshipConnector,
  val agentServicesAccountConnector: AgentServicesAccountConnector,
  val citizenDetailsConnector: CitizenDetailsConnector)
    extends GetClientName {

  def getRecentAgentInvitations(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    now: LocalDate): Future[Seq[TrackedInvitation]] =
    invitationsConnector
      .getAllInvitations(arn, now.minusDays(showLastDays))
      .map(_.filter(whitelistedInvitation(isPirWhitelisted))
        .map(TrackedInvitation.fromStored))
      .flatMap(ii => Future.sequence(ii.map(addClientName)))

  def whitelistedInvitation(isPirWhitelisted: Boolean): StoredInvitation => Boolean =
    i => isPirWhitelisted || i.service != Services.HMRCPIR

  def addClientName(
    invitation: TrackedInvitation)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[TrackedInvitation] =
    getClientNameByService(invitation).map(cn => invitation.copy(clientName = cn))

  def getInactiveClients(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Seq[InactiveClient]] = {
    val itsaRelationships: Future[Seq[ItsaTrackRelationship]] = relationshipsConnector.getInactiveItsaRelationships
    val vatRelationships: Future[Seq[VatTrackRelationship]] = relationshipsConnector.getInactiveVatRelationships
    val irvRelationships: Future[Seq[IrvTrackRelationship]] = pirRelationshipConnector.getInactiveIrvRelationships

    for {
      relationships <- Future.sequence(Seq(itsaRelationships, vatRelationships, irvRelationships)).map(_.flatten)

      inactiveClients <- Future.traverse(relationships) {
                          case ItsaTrackRelationship(_, dateTo, clientId) =>
                            for {
                              nino <- agentServicesAccountConnector.getNinoForMtdItId(MtdItId(clientId))
                              name <- getTradingName(nino)
                            } yield InactiveClient("HMRC-MTD-IT", name.getOrElse(""), dateTo)
                          case VatTrackRelationship(_, dateTo, clientId) =>
                            for {
                              vatName <- getVatName(Some(Vrn(clientId)))
                            } yield InactiveClient("HMRC-MTD-VAT", vatName.getOrElse(""), dateTo)
                          case IrvTrackRelationship(_, dateTo, clientId) =>
                            for {
                              citizenName <- getCitizenName(Some(Nino(clientId)))
                            } yield InactiveClient("PERSONAL-INCOME-RECORD", citizenName.getOrElse(""), dateTo)
                          case _ => Future successful InactiveClient("", "", None)
                        }
    } yield inactiveClients.filter(_.serviceName.nonEmpty)
  }

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

  def bindInvitationsAndRelationships(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier): Future[Seq[TrackInformationSorted]] = {
    implicit val now = LocalDate.now(DateTimeZone.UTC)
    for {
      invitations <- getRecentAgentInvitations(arn, isPirWhitelisted, showLastDays)
      trackInfoInvitations <- Future.traverse(invitations) {
                               case TrackedInvitation(service, _, _, clientName, status, _, expiryDate)
                                   if status == "Pending" || status == "Expired" =>
                                 Future successful TrackInformationSorted(
                                   service,
                                   clientName,
                                   effectiveStatus(status, expiryDate),
                                   None,
                                   Some(expiryDate))

                               case TrackedInvitation(service, _, _, clientName, status, lastUpdated, _) =>
                                 Future successful TrackInformationSorted(
                                   service,
                                   clientName,
                                   status,
                                   Some(LocalDate.parse(lastUpdated.toLocalDate.toString)),
                                   None)
                               case _ => Future successful TrackInformationSorted("", None, "", None, None)
                             }
      relationships <- getInactiveClients
      trackInfoRelationships <- Future.traverse(relationships) {
                                 case InactiveClient(service, clientName, dateTo) =>
                                   Future successful TrackInformationSorted(
                                     service,
                                     Some(clientName),
                                     "InvalidRelationship",
                                     dateTo,
                                     None)
                                 case _ => Future successful TrackInformationSorted("", None, "", None, None)
                               }
    } yield (trackInfoInvitations ++ trackInfoRelationships).sorted(TrackInformationSorted.orderingByDate)
  }

  def effectiveStatus(status: String, expiryDate: LocalDate)(implicit now: LocalDate): String =
    if (status == "Pending" && (now.isAfter(expiryDate) || now.isEqual(expiryDate))) "Expired"
    else status
}
