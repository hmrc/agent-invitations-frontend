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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.data.Forms.{text, tuple}
import play.api.data.Mapping
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.validateDateFields

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, ResolverStyle}

object DateFieldHelper {

  def validateDate(value: String): Boolean = if (parseDate(value)) true else false

  val dateTimeFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("uuuu-M-d").withResolverStyle(ResolverStyle.STRICT)

  def parseDate(date: String): Boolean =
    try {
      LocalDate.parse(date, dateTimeFormat)
      true
    } catch {
      case _: Throwable => false
    }

  def dateFieldsMapping(formMessageKey: String): Mapping[String] =
    tuple(
      "year"  -> text,
      "month" -> text,
      "day"   -> text
    ).verifying(validateDateFields(formMessageKey))
      .transform[String](
        { case (y, m, d) =>
          if (y.isEmpty || m.isEmpty || d.isEmpty) ""
          else LocalDate.of(y.toInt, m.toInt, d.toInt).format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
        },
        date =>
          try {
            val l = LocalDate.parse(date)
            (l.getYear.toString, l.getMonthValue.toString, l.getDayOfMonth.toString)
          } catch {
            case e: Exception => throw new IllegalArgumentException(s"unexpected date input pattern $e")
          }
      )

}
