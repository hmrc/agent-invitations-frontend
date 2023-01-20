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

package uk.gov.hmrc.agentinvitationsfrontend.models

import play.api.libs.json.{Format, Reads, Writes, __}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}

object MongoLocalDateTimeFormat {

  // LocalDate

  final val localDateReads: Reads[LocalDate] =
    Reads
      .at[String](__ \ "$date" \ "$numberLong")
      .map(date => Instant.ofEpochMilli(date.toLong).atZone(ZoneOffset.UTC).toLocalDate)

  final val localDateWrites: Writes[LocalDate] =
    Writes
      .at[String](__ \ "$date" \ "$numberLong")
      .contramap(_.atStartOfDay(ZoneOffset.UTC).toInstant.toEpochMilli.toString)

  // for data that exists prior to the hmrc-mongo migration
  final val legacyDateReads: Reads[LocalDate] =
    Reads
      .at[String](__)
      .map(date => LocalDate.parse(date))

  // LocalDateTime

  final val localDateTimeReads: Reads[LocalDateTime] =
    Reads
      .at[String](__ \ "$date" \ "$numberLong")
      .map { dateTime =>
        Instant.ofEpochMilli(dateTime.toLong).atZone(ZoneOffset.UTC).toLocalDateTime
      }

  // for data that exists prior to the hmrc-mongo migration - TODO remove this after legacy data expires (30 days)
  final val legacyDateTimeReads: Reads[LocalDateTime] =
    Reads
      .at[String](__)
      .map(dateTime => LocalDateTime.parse(dateTime))

  final val localDateTimeWrites: Writes[LocalDateTime] =
    Writes
      .at[String](__ \ "$date" \ "$numberLong")
      .contramap(_.toInstant(ZoneOffset.UTC).toEpochMilli.toString)

  final implicit val localDateTimeFormat: Format[LocalDateTime] =
    Format(localDateTimeReads.orElse(legacyDateTimeReads), localDateTimeWrites)

  final implicit val localDateFormat: Format[LocalDate] =
    Format(localDateReads.orElse(legacyDateReads), localDateWrites)

}
