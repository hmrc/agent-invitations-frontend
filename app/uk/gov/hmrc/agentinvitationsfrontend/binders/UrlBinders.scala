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

package uk.gov.hmrc.agentinvitationsfrontend.binders

import play.api.mvc.{PathBindable, QueryStringBindable}
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus
import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

object UrlBinders {

  implicit val invitationIdBinder: PathBindable[InvitationId] = getInvitationIdBinder

  def getInvitationIdBinder(implicit stringBinder: PathBindable[String]) = new PathBindable[InvitationId] {

    override def bind(key: String, value: String): Either[String, InvitationId] = {
      val isValidPrefix = value.headOption.fold(false)(Seq('A', 'B', 'C').contains)

      if (isValidPrefix && InvitationId.isValid(value))
        Right(InvitationId(value))
      else
        Left(ErrorConstants.InvitationIdNotFound)
    }

    override def unbind(key: String, id: InvitationId): String = stringBinder.unbind(key, id.value)
  }

  implicit val filterFormStatusBinder: QueryStringBindable[FilterFormStatus] = getFilterFormStatusBinder

  def getFilterFormStatusBinder(implicit queryStringBinder: QueryStringBindable[String]) =
    new QueryStringBindable[FilterFormStatus] {

      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, FilterFormStatus]] =
        for {
          status <- queryStringBinder.bind(key, params)
        } yield
          status match {
            case Right(s) =>
              try {
                Right(FilterFormStatus.toEnum(s))
              } catch {
                case e: Exception => Left(ErrorConstants.StatusError)
              }
            case _ => Left(ErrorConstants.StatusError)
          }

      override def unbind(key: String, ffs: FilterFormStatus): String =
        queryStringBinder.unbind(key, FilterFormStatus.fromEnum(ffs))
    }
}

object ErrorConstants {
  val InvitationIdNotFound = "INVITATION_ID_NOTFOUND"
  val StatusError = "FORM_FILTER_STATUS_INVALID"

}
