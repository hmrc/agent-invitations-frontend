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

package uk.gov.hmrc.agentinvitationsfrontend.views.track

import uk.gov.hmrc.agentinvitationsfrontend.models.{PageInfo, TrackInformationSorted}

case class TrackPageConfig(
  invitationsAndRelationships: Seq[TrackInformationSorted], // should be just one page of results
  trackRequestsShowLastDays: Int,
  cancelAuthActionFlag: Boolean,
  pageInfo: PageInfo,
  totalResults: Int) {

  val firstResult: Int = (pageInfo.page - 1) * pageInfo.resultsPerPage + 1
  val lastResult: Int = firstResult + invitationsAndRelationships.size - 1

  val numberOfPages: Int = Math.ceil(totalResults.toDouble / pageInfo.resultsPerPage).toInt
  val hasNextPage: Boolean = numberOfPages > 1 && pageInfo.page < numberOfPages

  val hasInvitationsOrRelationships: Boolean = invitationsAndRelationships.nonEmpty
}
