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

package uk.gov.hmrc.agentinvitationsfrontend.validators

import play.api.data.Forms.of
import play.api.data.Mapping
import play.api.data.format.Formats._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.DateFieldHelper.{dateFieldsMapping, validDobDateFormat, validateDate}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{FeatureFlags, ValidateHelper}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentmtdidentifiers.model.{Utr, Vrn}
import uk.gov.hmrc.domain.Nino

object Validators {
  val lowerCaseText: Mapping[String] = of[String].transform(_.trim.toLowerCase, identity)

  val trimmedUppercaseText: Mapping[String] = of[String].transform(_.trim.toUpperCase, identity)

  //Patterns
  val postcodeCharactersRegex = "^[a-zA-Z0-9 ]+$"

  val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)

  val uppercaseNormalizedText: Mapping[String] = normalizedText.transform(_.toUpperCase, identity)

  val vrnRegex = "[0-9]{9}"

  val ninoRegex = "[[A-Z]&&[^DFIQUV]][[A-Z]&&[^DFIQUVO]] ?\\d{2} ?\\d{2} ?\\d{2} ?[A-D]{1}"

  def validPostcode(
    isKfcFlagOn: Boolean,
    invalidFormatFailure: String,
    emptyFailure: String,
    invalidCharactersFailure: String) = Constraint[String] { input: String =>
    if (isKfcFlagOn) {
      if (input.isEmpty) Invalid(ValidationError(emptyFailure))
      else if (!input.matches(postcodeCharactersRegex)) Invalid(ValidationError(invalidCharactersFailure))
      else if (!input.matches(postcodeRegex)) Invalid(ValidationError(invalidFormatFailure))
      else Valid
    } else Valid
  }

  def postcodeMapping(featureEnabled: Boolean): Mapping[Option[String]] =
    optionalIf(
      featureEnabled,
      trimmedUppercaseText.verifying(
        validPostcode(
          featureEnabled,
          "enter-postcode.invalid-format",
          "error.postcode.required",
          "enter-postcode.invalid-characters"))
    )

  val validVrn =
    ValidateHelper.validateVrnField("error.vrn.required", "enter-vrn.regex-failure", "enter-vrn.checksum-failure")

  val validVatDateFormat: Constraint[String] =
    ValidateHelper.validateField("error.vat-registration-date.required", "enter-vat-registration-date.invalid-format")(
      vatRegistrationDate => validateDate(vatRegistrationDate))

  def validNino(nonEmptyFailure: String = "error.nino.required", invalidFailure: String = "enter-nino.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(nino => Nino.isValid(nino))

  def dateOfBirthMapping(showKfcPersonalIncome: Boolean): Mapping[Option[String]] =
    optionalIf(
      showKfcPersonalIncome,
      dateFieldsMapping(validDobDateFormat)
    )

  def validUtr(nonEmptyFailure: String = "error.utr.required", invalidFailure: String = "enter-utr.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(utr => Utr.isValid(utr))

  val validateClientId: Constraint[String] = Constraint[String] { fieldValue: String =>
    fieldValue match {
      case clientId if clientId.nonEmpty && clientId.matches(vrnRegex) =>
        if (Vrn.isValid(clientId)) Valid
        else Invalid(ValidationError("INVALID_VRN"))
      case clientId if clientId.nonEmpty && clientId.matches(ninoRegex) =>
        if (Nino.isValid(clientId)) Valid
        else Invalid(ValidationError("INVALID_NINO"))
      case _ =>
        Invalid(ValidationError(s"INVALID_CLIENT_ID_RECEIVED:${if (fieldValue.nonEmpty) fieldValue else "NOTHING"}"))
    }
  }

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

  def confirmationChoice(errorMessage: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError(errorMessage))
  }

  def vatRegDateMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))
}
