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

  case class TrackResultsPage(results: Seq[TrackInformationSorted], totalResults: Int, clientSet: Set[String])

  def bindInvitationsAndRelationships(
    arn: Arn,
    isPirWhitelisted: Boolean,
    showLastDays: Int,
    pageInfo: PageInfo,
    filterByClient: Option[String],
    filterByStatus: Option[String])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[TrackResultsPage] =
    allResults(arn, isPirWhitelisted, showLastDays).map { all =>
      val filteredResults = applyFilter(all)(filterByClient, filterByStatus)

      val from = (pageInfo.page - 1) * pageInfo.resultsPerPage
      val until = from + pageInfo.resultsPerPage
      val pageItems: Seq[TrackInformationSorted] = filteredResults.slice(from, until)

      TrackResultsPage(pageItems, filteredResults.size, all.flatMap(_.clientName).toSet)
    }

  def applyFilter(unfilteredResults: Seq[TrackInformationSorted])(
    filterByClient: Option[String],
    filterByStatus: Option[String]): Seq[TrackInformationSorted] =
    filterByClient.fold(unfilteredResults)(client => unfilteredResults.filter(_.clientName.getOrElse("") == client))

  /*
   * match each InvalidRelationship with corresponding Accepted and remove the latter so we don't show both events in the list, taking
   * care not to remove what could be one (and the latest) Accepted (and bearing in mind we have only config.showLastDays results).
   * */
  private def removeAcceptedForInvalid(allResults: Seq[TrackInformationSorted]) =
    allResults diff allResults.collect {
      case a: TrackInformationSorted if a.status == "InvalidRelationship" => {
        allResults
          .filter(
            b =>
              b.status == "Accepted" &&
                b.clientId == a.clientId &&
                b.service == a.service &&
                isOnOrBefore(b.date, a.date))
          .take(1)
      }
    }.flatten

  private def isOnOrBefore(a: Option[LocalDate], b: Option[LocalDate]) =
    try {
      !a.get.isAfter(b.get)
    } catch {
      case e: Exception => false
    }

  def allResults(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[TrackInformationSorted]] = {
    implicit val now = LocalDate.now(DateTimeZone.UTC)
    for {
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
      refinedResults = removeAcceptedForInvalid(trackInfoInvitations ++ trackInfoRelationships)
      finalResult <- Future.traverse(refinedResults) { trackInfo =>
                      for {
                        name <- getClientNameByService(trackInfo.clientId, trackInfo.service)
                      } yield trackInfo.copy(clientName = name)
                    }
    } yield
      finalResult
        .sorted(TrackInformationSorted.orderingByDate)
  }
}
