/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.data.Forms.{mapping, text}
import play.api.data.Mapping
import play.api.data.validation.Constraint

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

  def parseDateIntoFields(date: String): Option[(String, String, String)] =
    try {
      val l = dateTimeFormat.parseLocalDate(date)
      Some((l.getYear.toString, l.getMonthOfYear.toString, l.getDayOfMonth.toString))
    } catch {
      case NonFatal(_) => None
    }

  val formatDateFromFields: (String, String, String) => String = {
    case (y, m, d) =>
      if (y.isEmpty || m.isEmpty || d.isEmpty) ""
      else {
        val month = if (m.length == 1) "0" + m else m
        val day = if (d.length == 1) "0" + d else d
        s"$y-$month-$day"
      }
  }

  val validVatDateFormat: Constraint[String] =
    ValidateHelper.validateField("error.vat-registration-date.required", "enter-vat-registration-date.invalid-format")(
      vatRegistrationDate => validateDate(vatRegistrationDate))

  val validDobDateFormat: Constraint[String] =
    ValidateHelper.validateField("error.irv-date-of-birth.required", "enter-irv-date-of-birth.invalid-format")(
      vatRegistrationDate => validateDate(vatRegistrationDate))

  def dateFieldsMapping(constraintDate: Constraint[String]): Mapping[String] =
    mapping(
      "year"  -> text.verifying("error.year.invalid-format", y => y.isEmpty || y.matches("^[0-9]{1,4}$")),
      "month" -> text.verifying("error.month.invalid-format", m => m.isEmpty || m.matches("^[0-9]{1,2}$")),
      "day"   -> text.verifying("error.day.invalid-format", d => d.isEmpty || d.matches("^[0-9]{1,2}$"))
    )(formatDateFromFields)(parseDateIntoFields).verifying(constraintDate)

}
