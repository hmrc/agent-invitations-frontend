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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.govukfrontend.views.Aliases.{RadioItem, Text}

case class ClientTypePageConfig(backLinkUrl: String, submitCall: Call, showTrustFlag: Boolean, isForVat: Boolean = false, isForCgt: Boolean = false)(
  implicit messages: Messages) {

  val personalOption = Seq("personal" -> Messages("client-type.personal"))
  val businessOption = Seq("business" -> Messages("client-type.business"))
  val trustOption = Seq("trust"       -> Messages("client-type.trust"))

  val clientTypes: Seq[(String, String)] =
    if (showTrustFlag && !isForVat && !isForCgt) personalOption ++ businessOption ++ trustOption
    else if (showTrustFlag && isForCgt) personalOption ++ trustOption
    else personalOption ++ businessOption

  val clientTypesAsRadioItems: Seq[RadioItem] = {
    clientTypes.map(
      client =>
        RadioItem(
          id = Some(client._1),
          content = Text(client._2),
          value = Some(client._1)
      ))
  }

}
