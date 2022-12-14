/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.views.track

import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.agentinvitationsfrontend.models.{FilterFormStatus, FilterTrackRequests, PageInfo, TrackInformationSorted}
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes
import uk.gov.hmrc.agentinvitationsfrontend.forms.FilterTrackRequestsForm

case class TrackPageConfig(
  invitationsAndRelationships: Seq[TrackInformationSorted], // should be just one page of results
  trackRequestsShowLastDays: Int,
  cancelAuthActionFlag: Boolean,
  pageInfo: PageInfo,
  totalResults: Int,
  clientSet: Set[String],
  filterByClient: Option[String],
  filterByStatus: Option[FilterFormStatus],
  filterForm: Option[Form[FilterTrackRequests]])(implicit messages: Messages) {

  val firstResult: Int = (pageInfo.page - 1) * pageInfo.resultsPerPage + 1
  val lastResult: Int = firstResult + invitationsAndRelationships.size - 1

  val numberOfPages: Int = Math.ceil(totalResults.toDouble / pageInfo.resultsPerPage).toInt
  val hasNextPage: Boolean = numberOfPages > 1 && pageInfo.page < numberOfPages

  val hasInvitationsOrRelationships: Boolean = invitationsAndRelationships.nonEmpty
  val showFilterForm: Boolean = filterByClient.isDefined || filterByStatus.isDefined || totalResults > pageInfo.resultsPerPage
  val filterFormSubmitUrl: Call = routes.AgentsRequestTrackingController.submitFilterTrackRequests
  val additionalQueryParams: String =
    s"""${filterByClient.fold("")(clientName => s"&client=$clientName")}${filterByStatus.fold("")(status => s"&status=$status")}"""

  val form: Form[FilterTrackRequests] = filterForm.getOrElse(
    FilterTrackRequestsForm
      .form(Set()))

  val statuses: Seq[FilterFormStatus] = FilterFormStatus.statuses

  val clientSeq: Seq[(String, String)] = clientSet.map(client => (client, client)).toSeq
  val statusesSeq: Seq[(String, String)] = statuses.map(status => (s"$status", messages(s"recent-invitations.filter-status.$status")))

}
