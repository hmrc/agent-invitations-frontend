/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.repository

import play.api.Logging
import play.api.libs.json._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.cache.model.Id
import uk.gov.hmrc.cache.repository.CacheRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait MongoSessionStore[T] extends Logging {

  val sessionName: String
  val cacheRepository: CacheRepository

  def getSessionId(implicit hc: HeaderCarrier): Option[String] =
    hc.sessionId.map(_.value)

  def get(implicit reads: Reads[T], hc: HeaderCarrier, ec: ExecutionContext): Future[Either[String, Option[T]]] =
    getSessionId match {
      case Some(sessionId) ⇒
        cacheRepository
          .findById(Id(sessionId))
          .flatMap(_.flatMap(_.data))
          .flatMap {
            case Some(cache) =>
              (cache \ sessionName).asOpt[JsObject] match {
                case None => Right(None)
                case Some(obj) =>
                  obj.validate[T] match {
                    case JsSuccess(p, _) => Right(Some(p))
                    case JsError(errors) =>
                      val allErrors = errors.map(_._2.map(_.message).mkString(",")).mkString(",")
                      Left(allErrors)
                  }
              }
            case None => Right(None)
          }
          .recover {
            case e ⇒
              Left(e.getMessage)
          }

      case None ⇒
        Right(None)
    }

  def store(newSession: T)(implicit writes: Writes[T], hc: HeaderCarrier, ec: ExecutionContext): Future[Either[String, Unit]] =
    getSessionId match {
      case Some(sessionId) ⇒
        cacheRepository
          .createOrUpdate(Id(sessionId), sessionName, Json.toJson(newSession))
          .map[Either[String, Unit]] { dbUpdate ⇒
            if (dbUpdate.writeResult.inError) {
              Left(dbUpdate.writeResult.errmsg.getOrElse("unknown error during inserting session data in mongo"))
            } else {
              Right(())
            }
          }
          .recover {
            case e ⇒
              Left(e.getMessage)
          }

      case None ⇒
        Left(s"no sessionId found in the HeaderCarrier to store in mongo")
    }

  def delete()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Either[String, Unit]] =
    getSessionId match {
      case Some(sessionId) ⇒
        cacheRepository
          .removeById(Id(sessionId))
          .map[Either[String, Unit]] { dbUpdate ⇒
            if (dbUpdate.writeErrors.nonEmpty) {
              Left(dbUpdate.writeErrors.map(_.errmsg).mkString(","))
            } else {
              Right(())
            }
          }
          .recover {
            case e ⇒
              Left(e.getMessage)
          }

      case None ⇒
        Right(())
    }
}
