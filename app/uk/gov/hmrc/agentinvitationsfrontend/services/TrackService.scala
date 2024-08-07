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

import uk.gov.hmrc.agentinvitationsfrontend.connectors._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, MtdItId, Service, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{Instant, LocalDate, ZoneOffset}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TrackService @Inject() (
  relationshipsConnector: RelationshipsConnector,
  pirRelationshipConnector: PirRelationshipConnector,
  val acaConnector: AgentClientAuthorisationConnector,
  val citizenDetailsConnector: CitizenDetailsConnector
) extends GetClientName {

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
    showLastDays: Int,
    pageInfo: PageInfo,
    filterByClient: Option[String],
    filterByStatus: Option[FilterFormStatus]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[TrackResultsPage] =
    allResults(arn, showLastDays).map { all =>
      val filteredResults = applyFilter(all)(filterByClient, filterByStatus)
      val from = (pageInfo.page - 1) * pageInfo.resultsPerPage
      val until = from + pageInfo.resultsPerPage
      val pageItems: Seq[TrackInformationSorted] = filteredResults.slice(from, until)
      val clientNames: Set[String] = all.flatMap(_.clientName).toSet // we need all client names for filtering
      TrackResultsPage(pageItems, filteredResults.size, clientNames)
    }

  def applyFilter(
    unfilteredResults: Seq[TrackInformationSorted]
  )(filterByClient: Option[String], filterByStatus: Option[FilterFormStatus]): Seq[TrackInformationSorted] = {
    val maybeClientFiltered = filterByClient.fold(unfilteredResults)(client => unfilteredResults.filter(_.clientName.getOrElse("") == client))
    filterByStatus.fold(maybeClientFiltered)(status => maybeClientFiltered.filter(status.filterForStatus))
  }

  def clientNames(arn: Arn, showLastDays: Int)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Set[String]] =
    allResults(arn, showLastDays).map(r => r.flatMap(_.clientName).toSet)

  def allResults(arn: Arn, showLastDays: Int)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[TrackInformationSorted]] = {
    implicit val now = Instant.now().atZone(ZoneOffset.UTC).toLocalDate

    val futureInvitations = getRecentAgentInvitations(arn, showLastDays)
      .map(_.flatMap {
        case TrackedInvitation(ct, srv, cId, cidt, an, st, lu, exp, iid, ire, reb, altItsa) if st == "Pending" || st == "Expired" =>
          Some(TrackInformationSorted(ct, Some(srv), cId, cidt, an, st, None, Some(exp), Some(iid), ire, reb, Some(lu), altItsa))
        case TrackedInvitation(ct, srv, cId, cidt, an, st, lu, _, iid, ire, reb, altItsa) =>
          Some(TrackInformationSorted(ct, Some(srv), cId, cidt, an, st, Some(lu), None, Some(iid), ire, reb, Some(lu), altItsa))
        case _ => None
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
            dateTo.map(_.atStartOfDay()),
            None,
            None,
            isRelationshipEnded = true,
            None,
            None
          )
        case _ =>
          TrackInformationSorted(None, None, "", "", None, "", None, None, None, isRelationshipEnded = true, None, None)
      })

    for {
      invitations   <- futureInvitations
      relationships <- futureRelationships
      matched = matchAndDiscard(invitations ++ relationships)
      refinedResults = refineStatus(matched)
      (nameOk, nameEmpty) = refinedResults.partition(_.clientName.isDefined)
      nameRetrieved <- Future.traverse(nameEmpty) { trackInfo =>
                         logger.warn(
                           s"ClientName was not available in invitation store for ${trackInfo.clientId}," +
                             s" status: ${trackInfo.status} date: ${trackInfo.dateTime} isEnded ${trackInfo.isRelationshipEnded} service: ${trackInfo.service} getting it from DES"
                         )
                         trackInfo.service match {
                           case Some(service) => getClientNameByService(trackInfo.clientId, service).map(name => trackInfo.copy(clientName = name))
                           case None          => Future.successful(trackInfo.copy(clientName = None))
                         }
                       }
      finalResults = nameOk ++ nameRetrieved
    } yield finalResults.sorted(TrackInformationSorted.orderingByDate)
  }

  def getRecentAgentInvitations(arn: Arn, showLastDays: Int)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    now: LocalDate
  ): Future[Seq[TrackedInvitation]] =
    acaConnector
      .getAllInvitations(arn, now.minusDays(showLastDays))
      .map {
        _.map(TrackedInvitation.fromStored)
      }

  def getInactiveClients(implicit c: HeaderCarrier, ec: ExecutionContext): Future[Seq[InactiveClient]] =
    for {
      relationships <- Future
                         .sequence(Seq(relationshipsConnector.getInactiveRelationships, pirRelationshipConnector.getInactiveIrvRelationships))
                         .map(_.flatten)
      filteredRelationships = relationships.filter(rel => relationshipsConnector.isServiceEnabled(rel.service))

      inactiveClients <- Future.traverse(filteredRelationships) {

                           case r if r.service == Service.MtdIt =>
                             acaConnector
                               .getNinoForMtdItId(MtdItId(r.clientId))
                               .map(nino => InactiveClient(Some(r.clientType), Some(r.service), nino.fold("")(_.value), "ni", r.dateTo))

                           case rel: InactiveTrackRelationship =>
                             Future successful InactiveClient(
                               Some(rel.clientType),
                               Some(rel.service),
                               rel.clientId,
                               rel.service.supportedSuppliedClientIdType.id,
                               rel.dateTo
                             )

                           case IrvTrackRelationship(_, dateTo, clientId) =>
                             Future successful InactiveClient(Some("personal"), Some(Service.PersonalIncomeRecord), clientId, "ni", dateTo)

                           case _ => Future successful InactiveClient(None, None, "", "", None)
                         }
    } yield inactiveClients.filter(_.service.nonEmpty)

  /*This method will match an Accepted with an Invalid. It is based on the premise that for a given client,
  if there are n Accepted then there must be either n or n-1 Invalids. Revisit once we can depend on the
  isRelationshipEnded flag (see comment in APB-5115).
   * */
  def matchAndDiscard(a: Seq[TrackInformationSorted]): Seq[TrackInformationSorted] = {

    def matchAcceptedWithInvalids(a: Seq[TrackInformationSorted]) = {

      val invalid = a.filter(_.status == "InvalidRelationship").sorted(TrackInformationSorted.orderingByDate).reverse
      val accepted = a.filter(_.status == "Accepted")
      val deauthed = a.filter(_.status == "Deauthorised")
      val acceptedOrDeauthed = (accepted.toList ::: deauthed.toList)
        .sorted(TrackInformationSorted.orderingByDate)
        .reverse

      logDiscrepancy(invalid, deauthed)

      def _match(remain: List[TrackInformationSorted], acc: List[Option[TrackInformationSorted]]): List[Option[TrackInformationSorted]] =
        remain match {
          case Nil => acc
          case hd :: tl =>
            hd.status match {
              case "Pending" | "Rejected" | "Expired" | "Cancelled" | "Partialauth" => _match(tl, Some(hd) :: acc)
              case "Accepted" | "InvalidRelationship" | "Deauthorised" =>
                acceptedOrDeauthed
                  .filter(_.service == hd.service)
                  .map(Some(_))
                  .zipAll(
                    invalid
                      .filter(invalid => servicesMatch(invalid, hd))
                      .map(Some(_)),
                    None,
                    None
                  )
                  .find(_._1.contains(hd)) match {
                  case Some((Some(accepted), Some(invalid))) =>
                    _match(
                      tl,
                      Some(
                        accepted.copy(
                          dateTime = invalid.dateTime,
                          isRelationshipEnded = true,
                          relationshipEndedBy = accepted.relationshipEndedBy.orElse(Some("Agent"))
                        )
                      ) :: acc // assumed agent led de-auth-- setRelationshipEnded was not in place until APB-5115), should change to Client after 30 days (allow for change to take effect)
                    )
                  case Some((Some(active), None)) => _match(tl, Some(active) :: acc) // unmatched so must still be active
                  case None                       => _match(tl, None :: acc) // invalid found
                  case e =>
                    logger.error(
                      s"unexpected match result on the track page: $e accepted or deauthed" +
                        s"size: ${acceptedOrDeauthed.size} invalid size: ${invalid.size}"
                    )
                    _match(tl, None :: acc)
                }
            }
        }
      _match(a.toList, List.empty)
    }
    a.groupBy(_.clientId).mapValues(matchAcceptedWithInvalids(_).flatten).values.flatten.toSeq
  }

  private def refineStatus(unrefined: Seq[TrackInformationSorted]) =
    unrefined.map {
      case a: TrackInformationSorted
          if (a.status == "Accepted" || a.status == "Deauthorised") && a.isRelationshipEnded && a.relationshipEndedBy.isDefined =>
        a.copy(status = s"AcceptedThenCancelledBy${a.relationshipEndedBy.get}")
      case a: TrackInformationSorted if a.status == "Deauthorised" && !(a.isRelationshipEnded && a.relationshipEndedBy.isDefined) =>
        a.copy(status = "Accepted")

      case b: TrackInformationSorted => b
    }

  private def logDiscrepancy(invalid: Seq[TrackInformationSorted], deauthorised: Seq[TrackInformationSorted]) =
    invalid.length - deauthorised.length match {
      case 0 => logger.warn(s"Deauthed statuses IS EQUAL TO Invalid statuses (${deauthorised.length} is equal to ${invalid.length})")
      case n if n > 0 =>
        logger.warn(s"Deauthed statuses IS LESS THAN Invalid statuses (${deauthorised.length} is less than ${invalid.length})")
        logDiscrepancyDetail(invalid, deauthorised)
      case n if n < 0 =>
        logger.warn(s"Deauthed statuses IS GREATER THAN Invalid statuses (${deauthorised.length} is more than ${invalid.length})")
        logDiscrepancyDetail(invalid, deauthorised)
    }

  private def logDiscrepancyDetail(invalid: Seq[TrackInformationSorted], deauthorised: Seq[TrackInformationSorted]) = {
    val invalidAsId = invalid.map(_.clientId)
    val deauthedAsId = deauthorised.map(_.clientId)
    val deauthedButNotInvalid = (deauthedAsId diff invalidAsId).mkString(", ")
    val invalideButNotDeauthed = (invalidAsId diff deauthedAsId).mkString(", ")
    logger.warn(s"Deauthed contains $deauthedButNotInvalid not in Invalid")
    logger.warn(s"Invalid contains $invalideButNotDeauthed not in Deauthed")
  }

  private def servicesMatch(invalid: TrackInformationSorted, agentReq: TrackInformationSorted): Boolean =
    agentReq.service match {
      case Some(Service.CbcNonUk) => invalid.service.contains(Service.Cbc)
      case _                      => invalid.service == agentReq.service
    }

  case class TrackResultsPage(results: Seq[TrackInformationSorted], totalResults: Int, clientSet: Set[String])
}
