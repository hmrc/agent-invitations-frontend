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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import play.api.data.validation.Constraint
import play.api.data.{FormError, Mapping}

case class OptionalMappingIf[T](isOn: Boolean, wrapped: Mapping[T], val constraints: Seq[Constraint[Option[T]]] = Nil)
    extends Mapping[Option[T]] {

  override val format: Option[(String, Seq[Any])] = wrapped.format

  val key = wrapped.key

  def verifying(addConstraints: Constraint[Option[T]]*): Mapping[Option[T]] =
    this.copy(constraints = constraints ++ addConstraints.toSeq)

  def bind(data: Map[String, String]): Either[Seq[FormError], Option[T]] =
    if (isOn) {
      data.keys
        .filter(p => p == key || p.startsWith(key + ".") || p.startsWith(key + "["))
        .map(k =>
          data
            .get(k)
            .filterNot(_.isEmpty))
        .collectFirst { case Some(v) => v }
        .map { _ =>
          wrapped.bind(data).right.map(Some(_))
        }
        .getOrElse {
          wrapped.bind(data ++ Map(key -> "")).right.map(Some(_))
        }
        .right
        .flatMap(applyConstraints)
    } else Right(None)

  def unbind(value: Option[T]): Map[String, String] =
    value.map(wrapped.unbind).getOrElse(Map.empty)

  def unbindAndValidate(value: Option[T]): (Map[String, String], Seq[FormError]) = {
    val errors = collectErrors(value)
    value.map(wrapped.unbindAndValidate).map(r => r._1 -> (r._2 ++ errors)).getOrElse(Map.empty -> errors)
  }

  def withPrefix(prefix: String): Mapping[Option[T]] =
    copy(wrapped = wrapped.withPrefix(prefix))

  val mappings: Seq[Mapping[_]] = wrapped.mappings

}
