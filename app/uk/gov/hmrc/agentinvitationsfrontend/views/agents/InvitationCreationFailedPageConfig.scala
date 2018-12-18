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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import uk.gov.hmrc.agentinvitationsfrontend.models.AuthorisationRequest

trait InvitationCreationFailedPageConfig {
  def failedRequests: Set[AuthorisationRequest]
  def isAll: Boolean

  val hasSingleRequest: Boolean = failedRequests.map(_.service).size == 1
}

case class SomeInvitationCreationFailedPageConfig(requests: Set[AuthorisationRequest])
    extends InvitationCreationFailedPageConfig {

  override def failedRequests: Set[AuthorisationRequest] = requests.filter(_.state == AuthorisationRequest.FAILED)

  override def isAll: Boolean = false
}

case class AllInvitationCreationFailedPageConfig(requests: Set[AuthorisationRequest])
    extends InvitationCreationFailedPageConfig {

  override def failedRequests: Set[AuthorisationRequest] = requests.filter(_.state == AuthorisationRequest.FAILED)

  override def isAll: Boolean = true
}
