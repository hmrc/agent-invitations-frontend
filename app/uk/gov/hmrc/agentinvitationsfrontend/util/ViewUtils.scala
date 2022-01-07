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

package uk.gov.hmrc.agentinvitationsfrontend.util

import play.api.data.{Field, Form}
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.html.components.{RadioItem, Text}
//import uk.gov.hmrc.govukfrontend.views.Aliases.{ActionItem, Actions, HtmlContent, Key, SummaryListRow, Value}

object ViewUtils {

  def getYesNoStartOverRadioItems()(implicit messages: Messages): Seq[RadioItem] =
    Seq(
      RadioItem(
        content = Text(Messages("global.yes")),
        value = Some("true")
      ),
      RadioItem(
        content = Text(Messages("global.no-start-over")),
        value = Some("false")
      )
    )

  def errorPrefix(form: Form[_])(implicit messages: Messages): String =
    if (form.hasErrors || form.hasGlobalErrors) s"${messages("error.prefix")} " else ""

  // Assumes Seq(id/value, messageKey) - create RadioOption type for inputs to make this safer?
  def mapToRadioItems(field: Field, inputs: Seq[(String, String)])(implicit messages: Messages): Seq[RadioItem] =
    inputs.map(
      a => {
        RadioItem(
          id = Some(a._1),
          value = Some(a._1),
          checked = field.value.contains(a._1),
          content = Text(messages(a._2))
        )
      }
    )

//  def mapAnswerRowToSummaryListRow(field: Field, answerRow: Seq[AnswerRow])(implicit messages: Messages): Seq[SummaryListRow] =
//    answerRow.map(
//      a => {
//        SummaryListRow(
//          key = Key(
//            content = Text(a.question)
//          ),
//          value = Value(
//            content = HtmlContent(@for(line <- a.answerLines) { @line<br> })
//        ),
//        actions = Some(Actions(
//          items = Seq(
//            ActionItem(
//              href = a.changeLink,
//              content = Text(a.buttonText),
//              visuallyHiddenText = Some(a.question)
//            )
//          )
//        ))
//        )
//      }
//    )

}
