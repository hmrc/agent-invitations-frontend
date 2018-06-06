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

import play.api.data.validation.{Constraint, Invalid}
import play.api.data.{FormError, Mapping}

object MappingOps {

  class ErrorMappedToKeyMapping[T](
    wrapped: Mapping[T],
    errorToKey: Map[String, String] = Map.empty,
    val constraints: Seq[Constraint[T]] = Nil)
      extends Mapping[T] {

    val key = wrapped.key
    val mappings = wrapped.mappings
    override val format = wrapped.format

    override def bind(data: Map[String, String]): Either[Seq[FormError], T] =
      wrapped.bind(data).right.flatMap(applyConstraints)
    override def unbind(value: T): Map[String, String] = wrapped.unbind(value)
    override def unbindAndValidate(value: T): (Map[String, String], Seq[FormError]) = {
      val (data, errors) = wrapped.unbindAndValidate(value)
      (data, errors ++ collectErrors(value))
    }

    override def withPrefix(prefix: String): ErrorMappedToKeyMapping[T] =
      new ErrorMappedToKeyMapping(wrapped.withPrefix(prefix), errorToKey, constraints)

    override def verifying(additionalConstraints: Constraint[T]*): ErrorMappedToKeyMapping[T] =
      new ErrorMappedToKeyMapping(wrapped, errorToKey, constraints ++ additionalConstraints)

    def verifyingWithErrorMap(
      additionalConstraintsWithErrorMaps: (Constraint[T], Map[String, String])*): ErrorMappedToKeyMapping[T] = {
      val additionalErrorToKey = additionalConstraintsWithErrorMaps.map(_._2).reduce(_ ++ _)
      new ErrorMappedToKeyMapping(
        wrapped,
        errorToKey ++ additionalErrorToKey,
        constraints ++ additionalConstraintsWithErrorMaps.map(_._1))
    }

    override protected def collectErrors(t: T): Seq[FormError] =
      constraints
        .map(_(t))
        .collect {
          case Invalid(errors) => errors
        }
        .flatten
        .map(ve => FormError(errorToKey.getOrElse(ve.messages.head, key), ve.messages, ve.args))

  }

  def errorAwareMapping[T](t: Mapping[T]): ErrorMappedToKeyMapping[T] = new ErrorMappedToKeyMapping[T](t)

}
