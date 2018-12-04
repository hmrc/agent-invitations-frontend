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

package uk.gov.hmrc.agentinvitationsfrontend.services

import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait Cache[T] {
  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[T]]

  def updateWith(f: T => T)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[T] =
    for {
      entry <- fetch
      modifiedEntry = f(entry.get)
      _ <- save(modifiedEntry)
    } yield modifiedEntry

  def save(input: T)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[T]
}
