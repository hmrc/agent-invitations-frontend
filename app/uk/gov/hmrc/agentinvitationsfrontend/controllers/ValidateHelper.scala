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

import play.api.data.Mapping
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn

object ValidateHelper {

  def nonEmpty(failure: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.isEmpty) Invalid(ValidationError(failure)) else Valid
  }

  def validateField(emptyFailure: String, invalidFailure: String)(condition: String => Boolean) = Constraint[String] {
    fieldValue: String =>
      nonEmpty(emptyFailure)(fieldValue) match {
        case i: Invalid =>
          i
        case Valid =>
          if (condition(fieldValue.trim.toUpperCase))
            Valid
          else
            Invalid(ValidationError(invalidFailure))
      }
  }

  def validateVrnField(nonEmptyFailure: String, regexFailure: String) = Constraint[String] { fieldValue: String =>
    nonEmpty(nonEmptyFailure)(fieldValue) match {
      case i: Invalid =>
        i
      case Valid =>
        if (!Vrn.isValid(fieldValue))
          Invalid(ValidationError(regexFailure))
        else
          Valid
    }
  }

  def optionalIf[A](isOn: Boolean, mapping: Mapping[A]): Mapping[Option[A]] = OptionalMappingIf(isOn, mapping)

}
