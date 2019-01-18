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
import javax.inject.Inject
import play.api.data.Forms.{boolean, mapping, of, optional, text}
import play.api.data.format.Formats._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.{Form, Mapping}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.DateFieldHelper.{dateFieldsMapping, validDobDateFormat, validVatDateFormat}
import uk.gov.hmrc.agentinvitationsfrontend.controllers.ValidateHelper.optionalIf
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR, supportedClientTypes, _}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino

class AgentInvitationControllerSupport @Inject()(featureFlags: FeatureFlags) {

  import AgentInvitationControllerSupport._

  //Form values
  val agentInvitationIdentifyClientFormItsa: Form[UserInputNinoAndPostcode] =
    agentInvitationIdentifyClientFormItsa(featureFlags)

  val agentInvitationIdentifyClientFormIrv: Form[UserInputNinoAndDob] =
    agentInvitationIdentifyClientFormIrv(featureFlags)

  val agentInvitationIdentifyClientFormVat: Form[UserInputVrnAndRegDate] =
    agentInvitationIdentifyClientFormVat(featureFlags)

  val agentInvitationIdentifyKnownFactForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackGenericFormKnownFact(featureFlags)

  val agentInvitationPostCodeForm: Form[UserInputNinoAndPostcode] =
    agentInvitationPostCodeForm(featureFlags)

  val agentFastTrackPostcodeForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackKnownFactForm(featureFlags, postcodeMapping(featureFlags))

  val agentFastTrackDateOfBirthForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackKnownFactForm(featureFlags, dateOfBirthMapping(featureFlags))

  val agentFastTrackVatRegDateForm: Form[CurrentAuthorisationRequest] =
    agentFastTrackKnownFactForm(featureFlags, vatRegDateMapping(featureFlags))

  //Validators
  private def validNino(
    nonEmptyFailure: String = "error.nino.required",
    invalidFailure: String = "enter-nino.invalid-format") =
    ValidateHelper.validateField(nonEmptyFailure, invalidFailure)(nino => Nino.isValid(nino))

  private val validVrn =
    ValidateHelper.validateVrnField("error.vrn.required", "enter-vrn.regex-failure", "enter-vrn.checksum-failure")

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

  //Patterns
  private val postcodeCharactersRegex = "^[a-zA-Z0-9 ]+$"

  private val postcodeRegex = "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}$|BFPO\\s?[0-9]{1,5}$"

  def clientTypeFor(clientType: Option[String], service: String): Option[String] =
    clientType.orElse(service match {
      case "HMRC-MTD-IT"            => Some("personal")
      case "PERSONAL-INCOME-RECORD" => Some("personal")
      case _                        => None
    })

  //Constraints
  private val clientTypeChoice: Constraint[Option[String]] =
    radioChoice("error.client-type.required")

  private val serviceChoice: Constraint[Option[String]] =
    radioChoice("error.service.required")

  val detailsChoice: Constraint[Option[Boolean]] = Constraint[Option[Boolean]] { fieldValue: Option[Boolean] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError("error.confirmDetails.invalid"))
  }

  def radioChoice[A](invalidError: String): Constraint[Option[A]] = Constraint[Option[A]] { fieldValue: Option[A] =>
    if (fieldValue.isDefined)
      Valid
    else
      Invalid(ValidationError(invalidError))
  }

  private def confirmationChoice(errorMessage: String): Constraint[String] = Constraint[String] { fieldValue: String =>
    if (fieldValue.trim.nonEmpty)
      Valid
    else
      Invalid(ValidationError(errorMessage))
  }

  //Mappings
  def postcodeMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(
      featureFlags.showKfcMtdIt,
      trimmedUppercaseText.verifying(
        validPostcode(
          featureFlags.showKfcMtdIt,
          "enter-postcode.invalid-format",
          "error.postcode.required",
          "enter-postcode.invalid-characters"))
    )

  def dateOfBirthMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(featureFlags.showKfcPersonalIncome, dateFieldsMapping(validDobDateFormat))

  def vatRegDateMapping(featureFlags: FeatureFlags): Mapping[Option[String]] =
    optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))

  val trimmedUppercaseText: Mapping[String] = of[String].transform(_.trim.toUpperCase, identity)

  val lowerCaseText: Mapping[String] = of[String].transform(_.trim.toLowerCase, identity)

  //Forms
  val agentInvitationSelectClientTypeForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text).verifying(clientTypeChoice),
        "service"          -> text,
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, _, _, _) =>
        UserInputNinoAndPostcode(clientType, "", None, None)
      })({ user =>
        Some((user.clientType, "", None, None))
      })
    )
  }

  val clientTypeOnlyForm: Form[Option[String]] = Form(mapping("clientType" -> optional(text)
    .verifying("Unsupported Client Type", clientType => supportedClientTypes.contains(clientType)))(identity)(Some(_)))

  val agentInvitationServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> optional(text).verifying(serviceChoice),
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, service, _, _) =>
        UserInputNinoAndPostcode(clientType, service.getOrElse(""), None, None)
      })({ user =>
        Some((user.clientType, Some(user.service), None, None))
      }))
  }

  val agentInvitationBusinessServiceForm: Form[UserInputNinoAndPostcode] = {
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> optional(text).verifying(serviceChoice),
        "clientIdentifier" -> optional(normalizedText),
        "knownFact"        -> optional(text)
      )({ (clientType, service, _, _) =>
        UserInputNinoAndPostcode(clientType, service.getOrElse(""), None, None)
      })({ user =>
        Some((user.clientType, Some(user.service), None, None))
      }))
  }

  val serviceNameForm: Form[String] = Form(
    mapping("service" -> text.verifying("Unsupported Service", service => supportedServices.contains(service)))(
      identity)(Some(_)))

  val checkDetailsForm: Form[ConfirmForm] = Form[ConfirmForm](
    mapping("checkDetails" -> optional(boolean)
      .verifying(detailsChoice))(ConfirmForm.apply)(ConfirmForm.unapply))

  def agentInvitationIdentifyClientFormItsa(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validNino()),
        "knownFact" -> optionalIf(
          featureFlags.showKfcMtdIt,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showKfcMtdIt,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (clientType, service, clientIdentifier, postcode) =>
        UserInputNinoAndPostcode(clientType, service, Some(clientIdentifier.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

  def agentInvitationIdentifyClientFormIrv(featureFlags: FeatureFlags): Form[UserInputNinoAndDob] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validNino()),
        "knownFact" -> optionalIf(
          featureFlags.showKfcPersonalIncome,
          dateFieldsMapping(validDobDateFormat)
        )
      )({ (clientType, service, clientIdentifier, dob) =>
        UserInputNinoAndDob(clientType, service, Some(clientIdentifier.trim.toUpperCase()), dob)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.dob))
      }))

  def agentInvitationIdentifyClientFormVat(featureFlags: FeatureFlags): Form[UserInputVrnAndRegDate] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText.verifying(validVrn),
        "knownFact"        -> optionalIf(featureFlags.showKfcMtdVat, dateFieldsMapping(validVatDateFormat))
      )({ (clientType, service, clientIdentifier, registrationDate) =>
        UserInputVrnAndRegDate(clientType, service, Some(clientIdentifier.trim.toUpperCase()), registrationDate)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.registrationDate))
      }))

  def agentConfirmationForm(errorMessage: String): Form[Confirmation] =
    Form(
      mapping(
        "accepted" -> optional(normalizedText)
          .transform[String](_.getOrElse(""), s => Some(s))
          .verifying(confirmationChoice(errorMessage))
      )(choice => Confirmation(choice.toBoolean))(confirmation => Some(confirmation.choice.toString)))

  def agentInvitationPostCodeForm(featureFlags: FeatureFlags): Form[UserInputNinoAndPostcode] =
    Form(
      mapping(
        "clientType"       -> optional(text),
        "service"          -> text,
        "clientIdentifier" -> normalizedText,
        "knownFact" -> optionalIf(
          featureFlags.showKfcMtdIt,
          trimmedUppercaseText.verifying(
            validPostcode(
              featureFlags.showKfcMtdIt,
              "enter-postcode.invalid-format",
              "error.postcode.required",
              "enter-postcode.invalid-characters"))
        )
      )({ (clientType, service, nino, postcode) =>
        UserInputNinoAndPostcode(clientType, service, Some(nino.trim.toUpperCase()), postcode)
      })({ user =>
        Some((user.clientType, user.service, user.clientIdentifier.getOrElse(""), user.postcode))
      }))

  private val validateFastTrackForm: Constraint[CurrentAuthorisationRequest] =
    Constraint[CurrentAuthorisationRequest] { formData: CurrentAuthorisationRequest =>
      formData match {
        case CurrentAuthorisationRequest(Some("personal") | None, HMRCMTDIT, "ni", clientId, _, _)
            if Nino.isValid(clientId) =>
          Valid
        case CurrentAuthorisationRequest(Some("personal") | None, HMRCPIR, "ni", clientId, _, _)
            if Nino.isValid(clientId) =>
          Valid
        case CurrentAuthorisationRequest(_, HMRCMTDVAT, "vrn", clientId, _, _) if Vrn.isValid(clientId) => Valid
        case _                                                                                          => Invalid(ValidationError("INVALID_SUBMISSION"))
      }
    }

  def agentFastTrackKnownFactForm(
    featureFlags: FeatureFlags,
    knownFactMapping: Mapping[Option[String]]): Form[CurrentAuthorisationRequest] =
    Form(
      mapping(
        "clientType"           -> optional(text),
        "service"              -> text,
        "clientIdentifierType" -> text,
        "clientIdentifier"     -> normalizedText,
        "knownFact"            -> knownFactMapping
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientType, service, clientIdType, clientId, knownFact)
      })({ authorisationRequest =>
        Some(
          (
            authorisationRequest.clientType,
            authorisationRequest.service,
            authorisationRequest.clientIdentifierType,
            authorisationRequest.clientIdentifier,
            authorisationRequest.knownFact))
      }).verifying(validateFastTrackForm))

  val agentFastTrackForm: Form[CurrentAuthorisationRequest] =
    Form(
      mapping(
        "clientType" -> optional(
          lowerCaseText.verifying("UNSUPPORTED_CLIENT_TYPE", Set("personal", "business").contains _)),
        "service" -> text.verifying("UNSUPPORTED_SERVICE", service => supportedServices.contains(service)),
        "clientIdentifierType" -> text
          .verifying("UNSUPPORTED_CLIENT_ID_TYPE", clientType => supportedTypes.contains(clientType)),
        "clientIdentifier" -> normalizedText.verifying(validateClientId),
        "knownFact"        -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientTypeFor(clientType, service), service, clientIdType, clientId, knownFact)
      })({ authorisationRequest =>
        Some(
          (
            authorisationRequest.clientType,
            authorisationRequest.service,
            authorisationRequest.clientIdentifierType,
            authorisationRequest.clientIdentifier,
            authorisationRequest.knownFact))
      }).verifying(validateFastTrackForm))

  def agentFastTrackGenericFormKnownFact(featureFlags: FeatureFlags): Form[CurrentAuthorisationRequest] =
    Form(
      mapping(
        "clientType" -> optional(text),
        "service"    -> text.verifying("UNSUPPORTED_SERVICE", service => supportedServices.contains(service)),
        "clientIdentifierType" -> text
          .verifying("UNSUPPORTED_CLIENT_ID_TYPE", clientType => supportedTypes.contains(clientType)),
        "clientIdentifier" -> normalizedText.verifying(validateClientId),
        "knownFact"        -> optional(text)
      )({ (clientType, service, clientIdType, clientId, knownFact) =>
        CurrentAuthorisationRequest(clientType, service, clientIdType, clientId, knownFact)
      })({ authorisationRequest =>
        Some(
          (
            authorisationRequest.clientType,
            authorisationRequest.service,
            authorisationRequest.clientIdentifierType,
            authorisationRequest.clientIdentifier,
            authorisationRequest.knownFact))
      }))

  //Extractors
  object ClientForMtdItWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[String] = arg match {
      case (UserInputNinoAndPostcode(`personal`, HMRCMTDIT, Some(clientIdentifier), _), featureFlags)
          if featureFlags.showKfcMtdIt =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientForPirWithFlagOn {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputNinoAndPostcode(`personal`, HMRCPIR, Some(_), _), featureFlags)
          if featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientWithItsaOrPirFlagOff {
    def unapply(arg: (UserInputNinoAndPostcode, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputNinoAndPostcode(`personal`, _, Some(_), _), featureFlags)
          if !featureFlags.showKfcMtdIt || !featureFlags.showKfcPersonalIncome =>
        Some(())
      case _ => None
    }
  }

  object ClientForVatWithFlagOn {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[String] = arg match {
      case (UserInputVrnAndRegDate(_, HMRCMTDVAT, Some(clientIdentifier), _), featureFlags)
          if featureFlags.showKfcMtdVat =>
        Some(clientIdentifier)
      case _ => None
    }
  }

  object ClientWithVatFlagOff {
    def unapply(arg: (UserInputVrnAndRegDate, FeatureFlags)): Option[Unit] = arg match {
      case (UserInputVrnAndRegDate(_, _, Some(_), _), featureFlags) if !featureFlags.showKfcMtdVat =>
        Some(())
      case _ => None
    }
  }

  object CurrentInvitationInputItsaReady {
    def unapply(arg: CurrentAuthorisationRequest)(
      implicit featureFlags: FeatureFlags): Option[FastTrackItsaInvitation] =
      arg match {
        case CurrentAuthorisationRequest(_, HMRCMTDIT, "ni", clientIdentifier, postcodeOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcMtdIt || postcodeOpt.exists(
              _.matches(postcodeRegex))) =>
          Some(
            FastTrackItsaInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcMtdIt) postcodeOpt.map(Postcode) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputPirReady {
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[FastTrackPirInvitation] =
      arg match {
        case CurrentAuthorisationRequest(_, HMRCPIR, "ni", clientIdentifier, dobOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcPersonalIncome || dobOpt.exists(
              DateFieldHelper.validateDate)) =>
          Some(
            FastTrackPirInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcPersonalIncome) dobOpt.map(DOB) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputVatReady {
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[FastTrackVatInvitation] =
      arg match {
        case CurrentAuthorisationRequest(clientType, HMRCMTDVAT, "vrn", clientIdentifier, vatRegDateOpt, _)
            if clientType.isDefined && Vrn.isValid(clientIdentifier) && (!featureFlags.showKfcMtdVat || vatRegDateOpt
              .exists(DateFieldHelper.validateDate)) =>
          Some(
            FastTrackVatInvitation(
              clientType,
              Vrn(clientIdentifier),
              if (featureFlags.showKfcMtdVat) vatRegDateOpt.map(VatRegDate) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputNeedsClientIdentifier {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case CurrentAuthorisationRequest(clientType, service, _, clientIdentifier, _, _) =>
          service match {
            case HMRCMTDVAT if !Vrn.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCMTDVAT, "vrn", "", None))
            case HMRCMTDIT if !Nino.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCMTDIT, "ni", "", None))
            case HMRCPIR if !Nino.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCPIR, "ni", "", None))
            case _ => None
          }
        case _ => None
      }
  }

  object CurrentInvitationInputFromFastTrackNeedsClientType {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case input: CurrentAuthorisationRequest if input.fromFastTrack && input.clientType.isEmpty => Some(input)
        case _                                                                                     => None
      }
  }

  object CurrentInvitationInputNeedsClientType {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case input: CurrentAuthorisationRequest if !input.fromFastTrack && input.clientType.isEmpty => Some(input)
        case _                                                                                      => None
      }
  }

  object CurrentInvitationInputNeedsKnownFact {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case CurrentAuthorisationRequest(clientType, HMRCMTDVAT, _, _, Some(vatRegDate), true)
            if supportedClientTypes.contains(clientType) && !DateFieldHelper.validateDate(vatRegDate) =>
          Some(currentAuthorisationRequest.copy(knownFact = None))

        case CurrentAuthorisationRequest(`personal`, HMRCMTDIT, _, _, Some(postcode), true)
            if !postcode.matches(postcodeRegex) =>
          Some(currentAuthorisationRequest.copy(knownFact = None))

        case CurrentAuthorisationRequest(`personal`, HMRCPIR, _, _, Some(dob), true)
            if !DateFieldHelper.validateDate(dob) =>
          Some(currentAuthorisationRequest.copy(knownFact = None))

        case CurrentAuthorisationRequest(clientType, service, _, _, None, true)
            if supportedClientTypes.contains(clientType) && Services.supportedServices.contains(service) =>
          Some(currentAuthorisationRequest)

        case _ => None
      }
  }

  object CurrentInvitationInputNeedService {
    def unapply(currentAuthorisationRequest: CurrentAuthorisationRequest): Option[CurrentAuthorisationRequest] =
      currentAuthorisationRequest match {
        case CurrentAuthorisationRequest(clientType, service, _, _, _, _) if service.isEmpty =>
          //TODO Change when mandatory
          Some(currentAuthorisationRequest.copy(clientType = clientType))
        case _ => None
      }
  }

  object CurrentInvitationInputWithNonEmptyClientId {
    def unapply(current: CurrentAuthorisationRequest): Option[(Option[String], String, String)] =
      if (current.clientIdentifier.nonEmpty) Some((current.clientType, current.clientIdentifier, current.service))
      else None
  }
}

object AgentInvitationControllerSupport {

  val normalizedText: Mapping[String] = of[String].transform(_.replaceAll("\\s", ""), identity)

  private val vrnRegex = "[0-9]{9}"

  private val ninoRegex = "[[A-Z]&&[^DFIQUV]][[A-Z]&&[^DFIQUVO]] ?\\d{2} ?\\d{2} ?\\d{2} ?[A-D]{1}"

  private[controllers] val validateClientId: Constraint[String] = Constraint[String] { fieldValue: String =>
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

}
