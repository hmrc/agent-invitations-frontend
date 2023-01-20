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

package uk.gov.hmrc.agentinvitationsfrontend.util

import play.api.data.{Field, Form}
import play.api.i18n.Messages
import uk.gov.hmrc.agentinvitationsfrontend.views.components.RadioData
import uk.gov.hmrc.govukfrontend.views.Aliases.{Hint, HtmlContent}
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

  def mapToRadioItems(field: Field, inputs: Seq[RadioData])(implicit msgs: Messages): Seq[RadioItem] =
    inputs.map { (radioData: RadioData) =>
      RadioItem(
        content = Text(msgs(radioData.label)),
        value = Some(radioData.name),
        id = Some(radioData.id.getOrElse(radioData.name)),
        checked = radioData.isChecked.getOrElse(field.value.contains(radioData.name)),
        hint = radioData.hint.map(h => {
          Hint(content = HtmlContent(msgs(h)))
        })
      )
    }

  def isDayError(field: String, form: Form[_]): Boolean =
    if (form.errors(field).exists(error => error.message.contains("day"))) true else false

  def isMonthError(field: String, form: Form[_]): Boolean =
    if (form.errors(field).exists(error => error.message.contains("month"))) true else false

  def isYearError(field: String, form: Form[_]): Boolean =
    if (form.errors(field).exists(error => error.message.contains("year"))) true else false

  def isEmptyOrInvalidError(field: String, form: Form[_]): Boolean =
    if (form
          .errors(field)
          .exists(error => error.message.contains("invalid")) || form.errors(field).exists(error => error.message.contains("required"))) true
    else false

  def dateErrorMapping(field: String, form: Form[_]): Map[String, String] =
    if (isDayError(field, form) || isEmptyOrInvalidError(field, form)) {
      Map(field -> s"$field.day")
    } else if (isMonthError(field, form)) {
      Map(field -> s"$field.month")
    } else {
      Map(field -> s"$field.year")
    }

}
