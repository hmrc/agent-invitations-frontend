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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.govukfrontend.views.Aliases.{RadioItem, Text}

case class ClientTypePageConfig(backLinkUrl: String, submitCall: Call, availableClientTypes: Seq[ClientType] = ClientType.clientTypes)(
  implicit messages: Messages) {

  def radioFor(clientType: ClientType) = clientType match {
    case ClientType.Personal => "personal" -> Messages("client-type.personal")
    case ClientType.Business => "business" -> Messages("client-type.business")
    case ClientType.Trust    => "trust"    -> Messages("client-type.trust")
  }

  val clientTypesAsRadioItems: Seq[RadioItem] = {
    availableClientTypes
      .map(radioFor)
      .map(
        client =>
          RadioItem(
            id = Some(client._1),
            content = Text(client._2),
            value = Some(client._1)
        ))
  }

}
