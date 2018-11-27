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

package uk.gov.hmrc.agentinvitationsfrontend.views.clients
import uk.gov.hmrc.agentinvitationsfrontend.models.Consent
import uk.gov.hmrc.agentinvitationsfrontend.controllers.routes

case class CheckAnswersPageConfig(consents: Seq[Consent], agencyName: String, clientType: String, uid: String) {

  def changeUrl(serviceKey: String) =
    routes.ClientsMultiInvitationController.getMultiConfirmTermsIndividual(clientType, uid, serviceKey)

}