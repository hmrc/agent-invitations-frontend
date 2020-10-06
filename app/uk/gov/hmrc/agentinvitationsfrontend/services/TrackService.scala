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
import org.joda.time.{DateTimeZone, LocalDate}
import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TrackService @Inject()(
  relationshipsConnector: RelationshipsConnector,
  pirRelationshipConnector: PirRelationshipConnector,
  val acaConnector: AgentClientAuthorisationConnector,
  val citizenDetailsConnector: CitizenDetailsConnector)
    extends GetClientName {

  def getRecentAgentInvitations(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    now: LocalDate): Future[Seq[TrackedInvitation]] =
    acaConnector
      .getAllInvitations(arn, now.minusDays(showLastDays))
      .map { invitations =>
        invitations
          .filter(whitelistedInvitation(isPirWhitelisted))
          .map(TrackedInvitation.fromStored)
      }

  def whitelistedInvitation(isPirWhitelisted: Boolean): StoredInvitation => Boolean =
    i => isPirWhitelisted || i.service != Services.HMRCPIR

  def getInactiveClients(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Seq[InactiveClient]] =
    for {
      relationships <- Future
                        .sequence(
                          Seq(
                            relationshipsConnector.getInactiveRelationships,
                            pirRelationshipConnector.getInactiveIrvRelationships
                          ))
                        .map(_.flatten)
      filteredRelationships = relationships.filter(rel => relationshipsConnector.isServiceEnabled(rel.service))

      inactiveClients <- Future.traverse(filteredRelationships) {

                          case r if r.service == Services.HMRCMTDIT =>
                            for {
                              nino <- acaConnector.getNinoForMtdItId(MtdItId(r.clientId))
                            } yield
                              InactiveClient(
                                Some(r.clientType),
                                r.service,
                                nino.map(ni => ni.value).getOrElse(""),
                                "ni",
                                r.dateTo
                              )

                          case rel: InactiveTrackRelationship =>
                            Future successful InactiveClient(
                              Some(rel.clientType),
                              rel.service,
                              rel.clientId,
                              Services.clientIdType(rel.service),
                              rel.dateTo)

                          case IrvTrackRelationship(_, dateTo, clientId) =>
                            Future successful InactiveClient(
                              Some("personal"),
                              "PERSONAL-INCOME-RECORD",
                              clientId,
                              "ni",
                              dateTo)

                          case _ => Future successful InactiveClient(None, "", "", "", None)
                        }
    } yield inactiveClients.filter(_.serviceName.nonEmpty)

  def relationships(identifierOpt: Option[TaxIdentifier])(
    f: TaxIdentifier => Future[Seq[TrackRelationship]]): Future[Seq[TrackRelationship]] =
    identifierOpt match {
      case Some(identifier) => f(identifier)
      case None             => Future.successful(Seq.empty)
    }

  def names(identifierOpt: Option[TaxIdentifier])(f: TaxIdentifier => Future[Seq[String]]): Future[Seq[String]] =
    identifierOpt match {
      case Some(identifier) => f(identifier)
      case None             => Future.successful(Seq.empty)
    }

  def getTradingName(
    clientIdentifier: Option[Nino])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier match {
      case Some(s) => acaConnector.getTradingName(s)
      case None    => Future successful None
    }

  def getCitizenName(
    clientIdentifier: Option[Nino])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier match {
      case Some(s) => citizenDetailsConnector.getCitizenDetails(s).map(citizen => citizen.name)
      case None    => Future successful None
    }

  def getVatName(
    clientIdentifier: Option[Vrn])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier
      .map { vrn =>
        acaConnector.getCustomerDetails(vrn).map { customerDetails =>
          customerDetails.tradingName
            .orElse(customerDetails.organisationName)
            .orElse(customerDetails.individual.map(_.name))
        }
      }
      .getOrElse(Future.successful(None))

  case class TrackResultsPage(results: Seq[TrackInformationSorted], totalResults: Int)

  def bindInvitationsAndRelationships(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int, pageInfo: PageInfo)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[TrackResultsPage] = {
    implicit val now = LocalDate.now(DateTimeZone.UTC)
    val allResults = for {
      invitations <- getRecentAgentInvitations(arn, isPirWhitelisted, showLastDays)
      trackInfoInvitations <- Future.traverse(invitations) {

                               case TrackedInvitation(
                                   clientType,
                                   service,
                                   clientId,
                                   clientIdType,
                                   status,
                                   _,
                                   expiryDate,
                                   invitationId,
                                   isRelationshipEnded,
                                   relationshipEndedBy) if status == "Pending" || status == "Expired" =>
                                 Future successful TrackInformationSorted(
                                   clientType,
                                   service,
                                   clientId,
                                   clientIdType,
                                   None,
                                   status,
                                   None,
                                   Some(expiryDate),
                                   Some(invitationId),
                                   isRelationshipEnded,
                                   relationshipEndedBy)

                               case TrackedInvitation(
                                   clientType,
                                   service,
                                   clientId,
                                   clientIdType,
                                   status,
                                   lastUpdated,
                                   _,
                                   invitationId,
                                   isRelationshipEnded,
                                   relationshipEndedBy) =>
                                 Future successful TrackInformationSorted(
                                   clientType,
                                   service,
                                   clientId,
                                   clientIdType,
                                   None,
                                   status,
                                   Some(LocalDate.parse(lastUpdated.toLocalDate.toString)),
                                   None,
                                   Some(invitationId),
                                   isRelationshipEnded,
                                   relationshipEndedBy
                                 )
                               case _ =>
                                 Future successful TrackInformationSorted(
                                   None,
                                   "",
                                   "",
                                   "",
                                   None,
                                   "",
                                   None,
                                   None,
                                   None,
                                   false,
                                   None)
                             }
      relationships <- getInactiveClients
      trackInfoRelationships <- Future.traverse(relationships) {
                                 case InactiveClient(clientType, service, clientId, clientIdType, dateTo) =>
                                   Future successful TrackInformationSorted(
                                     clientType,
                                     service,
                                     clientId,
                                     clientIdType,
                                     None,
                                     "InvalidRelationship",
                                     dateTo,
                                     None,
                                     None,
                                     true,
                                     None)
                                 case _ =>
                                   Future successful TrackInformationSorted(
                                     None,
                                     "",
                                     "",
                                     "",
                                     None,
                                     "",
                                     None,
                                     None,
                                     None,
                                     true,
                                     None)
                               }
    } yield
      (trackInfoInvitations ++ trackInfoRelationships)
        .sorted(TrackInformationSorted.orderingByDate)

    allResults.flatMap { all =>
      // get one page of the results and fetch client names for each item

      val from = (pageInfo.page - 1) * pageInfo.resultsPerPage
      val until = from + pageInfo.resultsPerPage
      val pageItems: Seq[TrackInformationSorted] = all.slice(from, until)

      Future
        .traverse(pageItems) { trackInfo =>
          for {
            name <- getClientNameByService(trackInfo.clientId, trackInfo.service)
          } yield trackInfo.copy(clientName = name)
        }
        .map(TrackResultsPage(_, all.size))
    }

  }
}
