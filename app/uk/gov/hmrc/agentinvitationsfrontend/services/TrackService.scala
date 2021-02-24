/*
 * Copyright 2021 HM Revenue & Customs
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
import org.joda.time.{DateTime, DateTimeZone, LocalDate}
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

  def relationships(identifierOpt: Option[TaxIdentifier])(f: TaxIdentifier => Future[Seq[TrackRelationship]]): Future[Seq[TrackRelationship]] =
    identifierOpt match {
      case Some(identifier) => f(identifier)
      case None             => Future.successful(Seq.empty)
    }

  def names(identifierOpt: Option[TaxIdentifier])(f: TaxIdentifier => Future[Seq[String]]): Future[Seq[String]] =
    identifierOpt match {
      case Some(identifier) => f(identifier)
      case None             => Future.successful(Seq.empty)
    }

  def getTradingName(clientIdentifier: Option[Nino])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier match {
      case Some(s) => acaConnector.getTradingName(s)
      case None    => Future successful None
    }

  def getCitizenName(clientIdentifier: Option[Nino])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier match {
      case Some(s) => citizenDetailsConnector.getCitizenDetails(s).map(citizen => citizen.name)
      case None    => Future successful None
    }

  def getVatName(clientIdentifier: Option[Vrn])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[String]] =
    clientIdentifier
      .map { vrn =>
        acaConnector.getCustomerDetails(vrn).map { customerDetails =>
          customerDetails.tradingName
            .orElse(customerDetails.organisationName)
            .orElse(customerDetails.individual.map(_.name))
        }
      }
      .getOrElse(Future.successful(None))

  def bindInvitationsAndRelationships(
    arn: Arn,
    isPirWhitelisted: Boolean,
    showLastDays: Int,
    pageInfo: PageInfo,
    filterByClient: Option[String],
    filterByStatus: Option[FilterFormStatus])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[TrackResultsPage] =
    allResults(arn, isPirWhitelisted, showLastDays).map { all =>
      val filteredResults = applyFilter(all)(filterByClient, filterByStatus)
      val from = (pageInfo.page - 1) * pageInfo.resultsPerPage
      val until = from + pageInfo.resultsPerPage
      val pageItems: Seq[TrackInformationSorted] = filteredResults.slice(from, until)
      val clientNames: Set[String] = all.flatMap(_.clientName).toSet //we need all client names for filtering
      TrackResultsPage(pageItems, filteredResults.size, clientNames)
    }

  def applyFilter(unfilteredResults: Seq[TrackInformationSorted])(
    filterByClient: Option[String],
    filterByStatus: Option[FilterFormStatus]): Seq[TrackInformationSorted] = {
    val maybeClientFiltered = filterByClient.fold(unfilteredResults)(client => unfilteredResults.filter(_.clientName.getOrElse("") == client))
    filterByStatus.fold(maybeClientFiltered)(status => maybeClientFiltered.filter(status.filterForStatus))
  }

  def clientNames(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Set[String]] =
    allResults(arn, isPirWhitelisted, showLastDays).map(r => r.flatMap(_.clientName).toSet)

  def allResults(arn: Arn, isPirWhitelisted: Boolean, showLastDays: Int)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Seq[TrackInformationSorted]] = {
    implicit val now = LocalDate.now(DateTimeZone.UTC)

    val futureInvitations = getRecentAgentInvitations(arn, isPirWhitelisted, showLastDays)
      .map(_.map {
        case TrackedInvitation(ct, srv, cId, cidt, an, st, _, exp, iid, ire, reb) if st == "Pending" || st == "Expired" =>
          TrackInformationSorted(ct, srv, cId, cidt, an, st, None, Some(exp), Some(iid), ire, reb)
        case TrackedInvitation(ct, srv, cId, cidt, an, st, lu, _, iid, ire, reb) =>
          TrackInformationSorted(ct, srv, cId, cidt, an, st, Some(lu), None, Some(iid), ire, reb)
        case _ => TrackInformationSorted(None, "", "", "", None, "", None, None, None, false, None)
      })

    val futureRelationships = getInactiveClients
      .map(_.map {
        case InactiveClient(clientType, service, clientId, clientIdType, dateTo) =>
          TrackInformationSorted(
            clientType,
            service,
            clientId,
            clientIdType,
            None,
            "InvalidRelationship",
            dateTo.map(_.toDateTimeAtStartOfDay),
            None,
            None,
            true,
            None)
        case _ =>
          TrackInformationSorted(None, "", "", "", None, "", None, None, None, true, None)
      })

    for {
      invitations   <- futureInvitations
      relationships <- futureRelationships
      matched = matchAndDiscard(invitations ++ relationships)
      refinedResults = refineStatus(matched)
      (nameOk, nameEmpty) = refinedResults.partition(_.clientName.isDefined)
      nameRetrieved <- Future.traverse(nameEmpty) { trackInfo =>
                        logger.warn(s"ClientName was not available in invitation store for ${trackInfo.clientId}," +
                          s" status: ${trackInfo.status} date: ${trackInfo.dateTime} isEnded ${trackInfo.isRelationshipEnded} service: ${trackInfo.service} getting it from DES")
                        getClientNameByService(trackInfo.clientId, trackInfo.service).map(name => trackInfo.copy(clientName = name))
                      }
      finalResults = nameOk ++ nameRetrieved
    } yield finalResults.sorted(TrackInformationSorted.orderingByDate)
  }

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
                        .sequence(Seq(relationshipsConnector.getInactiveRelationships, pirRelationshipConnector.getInactiveIrvRelationships))
                        .map(_.flatten)
      filteredRelationships = relationships.filter(rel => relationshipsConnector.isServiceEnabled(rel.service))

      inactiveClients <- Future.traverse(filteredRelationships) {

                          case r if r.service == Services.HMRCMTDIT =>
                            acaConnector
                              .getNinoForMtdItId(MtdItId(r.clientId))
                              .map(nino => InactiveClient(Some(r.clientType), r.service, nino.fold("")(_.value), "ni", r.dateTo))

                          case rel: InactiveTrackRelationship =>
                            Future successful InactiveClient(
                              Some(rel.clientType),
                              rel.service,
                              rel.clientId,
                              Services.clientIdType(rel.service),
                              rel.dateTo)

                          case IrvTrackRelationship(_, dateTo, clientId) =>
                            Future successful InactiveClient(Some("personal"), "PERSONAL-INCOME-RECORD", clientId, "ni", dateTo)

                          case _ => Future successful InactiveClient(None, "", "", "", None)
                        }
    } yield inactiveClients.filter(_.serviceName.nonEmpty)

  private def matchAndDiscard(a: Seq[TrackInformationSorted]): Seq[TrackInformationSorted] = {

    def transform(a: Seq[TrackInformationSorted]) = {

      val accepted = a.filter(_.status == "Accepted")
      val invalid = a.filter(_.status == "InvalidRelationship")

      def matchD(remain: List[TrackInformationSorted], acc: List[Option[TrackInformationSorted]]): List[Option[TrackInformationSorted]] =
        remain match {
          case Nil => acc
          case hd :: tl =>
            hd.status match {
              case "Pending" | "Rejected" | "Expired" | "Cancelled" => matchD(tl, Some(hd) :: acc)
              case "Accepted" | "InvalidRelationship" => {

                accepted.map(Some(_)).zipAll(invalid.map(Some(_)), None, None).find(_._1.contains(hd)) match {
                  case Some((Some(accepted), Some(invalid))) =>
                    matchD(
                      tl,
                      Some(
                        accepted.copy(
                          dateTime = invalid.dateTime,
                          isRelationshipEnded = true,
                          relationshipEndedBy = accepted.relationshipEndedBy.orElse(Some("Agent")))) :: acc
                    )
                  case Some((Some(_), None)) => matchD(tl, None :: acc)
                  case None                  => matchD(tl, None :: acc)
                  case e                     => throw new Exception(s"possibly more Accepted than Invalid--should not happen. Error: $e")
                }
              }
            }
        }
      matchD(a.toList, List.empty)
    }

    a.groupBy(_.clientId).mapValues(x => transform(x).flatten).values.flatten.toSeq
  }

  private def isOnOrAfter(a: Option[DateTime], that: Option[DateTime]) =
    a.flatMap(x => that.map(y => !x.toLocalDate.isBefore(y.toLocalDate))).getOrElse(false)

//  private def isOnOrBefore(a: Option[DateTime], that: Option[DateTime]) =
//    a.flatMap(x => that.map(y => !x.toLocalDate.isAfter(y.toLocalDate))).getOrElse(false)

//  private def isEqual(a: Option[DateTime], that: Option[DateTime]) =
//    a.flatMap(x => that.map(y => x.isEqual(y))).getOrElse(false)

  private def refineStatus(unrefined: Seq[TrackInformationSorted]) =
    unrefined.map {
      case a: TrackInformationSorted if a.status == "Accepted" && a.isRelationshipEnded && a.relationshipEndedBy.isDefined => {
        a.copy(status = s"AcceptedThenCancelledBy${a.relationshipEndedBy.get}")
      }
      case b: TrackInformationSorted => b
    }

  def maybeUpdateStatus(a: TrackInformationSorted, sorteds: Seq[TrackInformationSorted]) = {
    val (accepted, invalid) = sorteds.toList
      .filter(
        x =>
          x.status == "Accepted" || a.status == "InvalidRelationship" &&
            x.clientId == a.clientId &&
            x.service == a.service &&
            isOnOrAfter(x.dateTime, a.dateTime))
      .sorted(TrackInformationSorted.orderingByDate)
      .partition(_.status == "Accepted")
    if (invalid.isEmpty) {
      println(s">>>>>>>>>INVALID EMPTY")
      None
    } // there is no invalid for this client so it must still be active
    else if (accepted.size > invalid.size) accepted.head match {
      case x if x == a => None // this is most recent and there is no invalid pair so it is assumed to still be valid
      case x if !x.isRelationshipEnded =>
        Some(a.copy(isRelationshipEnded = true, relationshipEndedBy = Some("Agent"), dateTime = invalid.last.dateTime))
      case _ => None
    } else Some(a.copy(isRelationshipEnded = true, relationshipEndedBy = Some("Agent"), dateTime = invalid.last.dateTime))
  }

  def mostRecentFor(a: TrackInformationSorted, seq: Seq[TrackInformationSorted]): Option[TrackInformationSorted] =
    seq
      .sorted(TrackInformationSorted.orderingByDate)
      .find(
        x =>
          x.status == "Accepted" &&
            x.clientId == a.clientId &&
            x.service == a.service &&
            !x.isRelationshipEnded &&
            isOnOrAfter(x.dateTime, a.dateTime))

  case class TrackResultsPage(results: Seq[TrackInformationSorted], totalResults: Int, clientSet: Set[String])
}
