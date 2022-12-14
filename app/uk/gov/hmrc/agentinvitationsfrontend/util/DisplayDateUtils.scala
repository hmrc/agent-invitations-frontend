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

import play.api.mvc.Request

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.util.Try

object DisplayDateUtils {

  val welshMonthLookup = Map(
    "January"   -> "Ionawr",
    "February"  -> "Chwefror",
    "March"     -> "Mawrth",
    "April"     -> "Ebrill",
    "May"       -> "Mai",
    "June"      -> "Mehefin",
    "July"      -> "Gorffennaf",
    "August"    -> "Awst",
    "September" -> "Medi",
    "October"   -> "Hydref",
    "November"  -> "Tachwedd",
    "December"  -> "Rhagfyr"
  )

  private val dateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("d MMMM uuuu", Locale.UK)

  private val dateFormatterLeadingZero: DateTimeFormatter =
    DateTimeFormatter.ofPattern("dd MMMM uuuu", Locale.UK)

  def displayDateForLang(date: Option[java.time.LocalDate], df: DateTimeFormatter = dateFormatter)(implicit request: Request[_]): String =
    date.fold("")(d => {
      val lang = request.cookies
        .get("PLAY_LANG")
        .map(_.value)
        .getOrElse("en")

      val dateStrEnglish = d.format(df)

      if (lang == "cy") {
        val monthStrEnglish = dateStrEnglish.split(' ')(1).trim
        val cyMonth = welshMonthLookup(monthStrEnglish)
        dateStrEnglish.replace(monthStrEnglish, cyMonth)
      } else dateStrEnglish
    })

  def displayDateForLangFromString(strDate: String)(implicit request: Request[_]): String = {
    val localDate = Try(LocalDate.parse(strDate))
    localDate.map(d => displayDateForLang(Some(d))).getOrElse(strDate)
  }

  def displayDateForLangWithLeadingZero(localDate: LocalDate)(implicit request: Request[_]): String =
    displayDateForLang(Some(localDate), dateFormatterLeadingZero)
}
