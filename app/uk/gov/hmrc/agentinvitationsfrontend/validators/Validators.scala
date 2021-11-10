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

package uk.gov.hmrc.agentinvitationsfrontend.validators

import play.api.data.Forms.{of, optional, text}
import play.api.data.Mapping
import play.api.data.format.Formats._
import play.api.data.validation._
import uk.gov.hmrc.agentinvitationsfrontend.controllers.DateFieldHelper.{parseDate, validateDate}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper
import uk.gov.hmrc.agentinvitationsfrontend.models.FilterFormStatus
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, PptRef, Vrn}
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

  val utrPattern = "^\\d{10}$"

  val urnPattern = "^((?i)[a-z]{2}trust[0-9]{8})$"

  def validPostcode(invalidFormatFailure: String, emptyFailure: String, invalidCharactersFailure: String): Constraint[String] =
    Constraint[String] { input: String =>
      if (input.isEmpty) Invalid(ValidationError(emptyFailure))
      else if (!input.matches(postcodeCharactersRegex)) Invalid(ValidationError(invalidCharactersFailure))
      else if (!input.matches(postcodeRegex)) Invalid(ValidationError(invalidFormatFailure))
      else Valid
    }

  def postcodeMapping: Mapping[String] =
    trimmedUppercaseText.verifying(
      validPostcode("enter-postcode.invalid-format", "error.postcode.required", "enter-postcode.invalid-characters")
    )

  val filterStatusValidation: Mapping[Option[String]] = optional(text).verifying(validFilterStatus)

  private def validFilterStatus: Constraint[Option[String]] = Constraint { fieldValue: Option[String] =>
    if (fieldValue.isDefined) {
      val o = fieldValue.get.trim
      nonEmptyWithMessage("recent-invitations.error.status-empty")(o) match {
        case Valid =>
          try {
            FilterFormStatus.toEnum(o)
            Valid
          } catch {
            case _: Exception => Invalid(ValidationError("recent-invitations.error.status-invalid"))
          }
        case i: Invalid => i
      }
    } else Valid
  }

  def filterClientValidation(clients: Set[String]): Mapping[Option[String]] =
    optional(text).verifying(validClient(clients))

  private def validClient(clients: Set[String]): Constraint[Option[String]] = Constraint { fieldValue: Option[String] =>
    if (fieldValue.isDefined) {
      val o = fieldValue.get.trim
      nonEmptyWithMessage("recent-invitations.error.client-empty")(o) match {
        case Valid =>
          if (clients.contains(o)) Valid else Invalid(ValidationError("recent-invitations.error.client-invalid"))
        case i: Invalid => i
      }
    } else Valid
  }

  def countryCode(validCountryCodes: Set[String]): Mapping[String] = text.verifying(validCountryCode(validCountryCodes))

  private def validCountryCode(codes: Set[String]) = Constraint { fieldValue: String =>
    val code = fieldValue.trim
    nonEmptyWithMessage("error.country.empty")(code) match {
      case i: Invalid => i
      case Valid =>
        if (codes.contains(code) && code != "GB" && code.length == 2) {
          Valid
        } else {
          Invalid(ValidationError("error.country.invalid"))
        }
    }
  }

  // Same as play.api.data.validation.Constraints.nonEmpty but with a custom message instead of error.required
  private def nonEmptyWithMessage(messageKey: String): Constraint[String] = Constraint[String] { (o: String) =>
    if (o == null) Invalid(ValidationError(messageKey))
    else if (o.trim.isEmpty) Invalid(ValidationError(messageKey))
    else Valid
  }

  val dayFieldRegex = "^[0-9]{1,4}$"
  val monthFieldRegex = "^(0?[1-9]|1[012])$"
  val yearFieldRegex = "^[0-9]{1,4}$"
  val year = "year"
  val month = "month"
  val day = "day"

  val validVrn: Constraint[String] =
    ValidateHelper.validateVrnField("error.vrn.required", "enter-vrn.regex-failure")

  def validateDateFields(formMessageKey: String) =
    Constraint[(String, String, String)] { s: (String, String, String) =>
      (s._1.matches(yearFieldRegex), s._2.matches(monthFieldRegex), s._3.matches(dayFieldRegex)) match {
        //y   //m   //d
        case a @ (true, true, true) =>
          if (parseDate(s"${s._1}-${s._2}-${s._3}")) Valid
          else invalid(s"enter-$formMessageKey-date.invalid-format", s"$day-$month-$year")
        case (true, true, false)  => invalid(s"error.$formMessageKey-date.day", day)
        case (true, false, false) => invalid(s"error.$formMessageKey-date.day-month", s"$day-$month-$year")
        case (true, false, true)  => invalid(s"error.$formMessageKey-date.month", month)
        case (false, true, true)  => invalid(s"error.$formMessageKey-date.year", year)
        case (false, false, true) =>
          invalid(s"error.$formMessageKey-date.month-year", s"$day-$month-$year")
        case (false, true, false) => invalid(s"error.$formMessageKey-date.day-year", s"$day-$month-$year")
        case (false, false, false) =>
          invalid(
            if (s._1.isEmpty && s._2.isEmpty && s._3.isEmpty) s"error.$formMessageKey-date.required"
            else s"enter-$formMessageKey-date.invalid-format",
            s"$day-$month-$year"
          )
      }
    }

  private def invalid(messageKey: String, inputFieldClass: String): Invalid =
    Invalid(ValidationError(messageKey, "inputFieldClass" -> inputFieldClass))

  val validVatDateFormat: Constraint[String] =
    ValidateHelper.validateField("error.vat-registration-date.required", "enter-vat-registration-date.invalid-format")(vatRegistrationDate =>
      validateDate(vatRegistrationDate))

  val validNino: Constraint[String] =
    ValidateHelper.validateField("error.nino.required", "enter-nino.invalid-format")(nino => Nino.isValid(nino))

  def validTrustTaxId(urnEnabled: Boolean) = if (urnEnabled) validTrustTaxIdentifier() else validUtr()

  def validUtr(): Constraint[String] =
    patternConstraint(utrPattern, "error.utr.required", "enter-utr.invalid-format")

  def validTrustTaxIdentifier(): Constraint[String] =
    validateTrustTaxIdentifier("error.urn.required", "enter-urn.invalid-format")

  def validCgtRef(): Constraint[String] =
    patternConstraint(CgtRef.cgtRegex, s"error.cgt.required", s"enter-cgt.invalid-format")

  val validPptRef: Constraint[String] =
    Constraint[String] { fieldValue: String =>
      val formattedField = fieldValue.replace(" ", "").trim
      Constraints.nonEmpty.apply(formattedField) match {
        case _: Invalid                          => Invalid(ValidationError("error.pptref.required"))
        case _ if PptRef.isValid(formattedField) => Valid
        case _                                   => Invalid("error.pptref.invalid-format")
      }
    }

  private def validateTrustTaxIdentifier(nonEmptyFailure: String, invalidFailure: String): Constraint[String] =
    Constraint[String] { fieldValue: String =>
      val formattedField = fieldValue.replace(" ", "").trim
      Constraints.nonEmpty.apply(formattedField) match {
        case _: Invalid                              => Invalid(ValidationError(nonEmptyFailure))
        case _ if formattedField.matches(utrPattern) => Valid
        case _ if formattedField.matches(urnPattern) => Valid
        case _                                       => Invalid(invalidFailure)
      }
    }

  private def patternConstraint(pattern: String, nonEmptyFailure: String, invalidFailure: String): Constraint[String] =
    Constraint[String] { fieldValue: String =>
      val formattedField = fieldValue.replace(" ", "").trim
      Constraints.nonEmpty.apply(formattedField) match {
        case _: Invalid => Invalid(ValidationError(nonEmptyFailure))
        case _ if formattedField.matches(pattern) =>
          Valid
        case _ => Invalid(invalidFailure)
      }
    }

  val validateClientId: Constraint[String] = Constraint[String] { fieldValue: String =>
    fieldValue match {
      case clientId if clientId.nonEmpty && clientId.matches(ninoRegex) =>
        if (Nino.isValid(clientId)) Valid
        else Invalid(ValidationError("INVALID_NINO"))
      case clientId if clientId.nonEmpty && clientId.matches(vrnRegex) && Vrn.isValid(clientId) => Valid
      case clientId if clientId.nonEmpty && clientId.matches(utrPattern)                        => Valid
      case clientId if clientId.nonEmpty && clientId.matches(urnPattern)                        => Valid
      case clientId if clientId.nonEmpty && CgtRef.isValid(clientId)                            => Valid
      case clientId if clientId.nonEmpty && PptRef.isValid(clientId)                            => Valid
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

  def confirmationChoice(errorMessageKey: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (Seq("true", "false").contains(fieldValue.trim))
      Valid
    else
      Invalid(ValidationError(errorMessageKey))
  }
}
