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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import org.joda.time.format.DateTimeFormat
import play.api.data.Forms.{mapping, text, tuple}
import play.api.data.Mapping
import play.api.data.validation.Constraint
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.validateDateFields

import scala.util.control.NonFatal

object DateFieldHelper {

  def validateDate(value: String): Boolean = if (parseDate(value)) true else false

  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd")

  def parseDate(date: String): Boolean =
    try {
      dateTimeFormat.parseDateTime(date)
      true
    } catch {
      case _: Throwable => false
    }

  def dateFieldsMapping(formMessageKey: String) =
    tuple(
      "year"  -> text,
      "month" -> text,
      "day"   -> text
    ).verifying(validateDateFields(formMessageKey))
      .transform[String](
        {
          case (y, m, d) =>
            if (y.isEmpty || m.isEmpty || d.isEmpty) ""
            else {
              val month = if (m.length == 1) "0" + m else m
              val day = if (d.length == 1) "0" + d else d
              s"$y-$month-$day"
            }
        },
        date =>
          try {
            val l = dateTimeFormat.parseLocalDate(date)
            (l.getYear.toString, l.getMonthOfYear.toString, l.getDayOfMonth.toString)
          } catch {
            case e: Exception => throw new IllegalArgumentException(s"unexpected date input pattern $e")
        }
      )

}
