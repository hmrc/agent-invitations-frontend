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
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR, supportedClientTypes, _}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.validators.Validators.postcodeRegex
import uk.gov.hmrc.agentmtdidentifiers.model.{Utr, Vrn}
import uk.gov.hmrc.domain.Nino

class AgentInvitationControllerSupport @Inject()(featureFlags: FeatureFlags)

object AgentInvitationControllerSupport {

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
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[ItsaInvitation] =
      arg match {
        case CurrentAuthorisationRequest(_, HMRCMTDIT, "ni", clientIdentifier, postcodeOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcMtdIt || postcodeOpt.exists(
              _.matches(postcodeRegex))) =>
          Some(
            ItsaInvitation(
              Nino(clientIdentifier),
              if (featureFlags.showKfcMtdIt) postcodeOpt.map(Postcode(_)) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputPirReady {
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[PirInvitation] =
      arg match {
        case CurrentAuthorisationRequest(_, HMRCPIR, "ni", clientIdentifier, dobOpt, _)
            if Nino.isValid(clientIdentifier) && (!featureFlags.showKfcPersonalIncome || dobOpt.exists(
              DateFieldHelper.validateDate)) =>
          Some(
            PirInvitation(Nino(clientIdentifier), if (featureFlags.showKfcPersonalIncome) dobOpt.map(DOB(_)) else None))
        case _ => None
      }
  }

  object CurrentInvitationInputVatReady {
    def unapply(arg: CurrentAuthorisationRequest)(implicit featureFlags: FeatureFlags): Option[VatInvitation] =
      arg match {
        case CurrentAuthorisationRequest(clientType, HMRCMTDVAT, "vrn", clientIdentifier, vatRegDateOpt, _)
            if clientType.isDefined && Vrn.isValid(clientIdentifier) && (!featureFlags.showKfcMtdVat || vatRegDateOpt
              .exists(DateFieldHelper.validateDate)) =>
          Some(
            VatInvitation(
              clientType,
              Vrn(clientIdentifier),
              if (featureFlags.showKfcMtdVat) vatRegDateOpt.map(VatRegDate(_)) else None))
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
            case HMRCNIORG if !Utr.isValid(clientIdentifier) =>
              Some(CurrentAuthorisationRequest(clientType, HMRCNIORG, "utr", "", None))
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
    def unapply(current: CurrentAuthorisationRequest): Option[(Option[String], String, String, Option[String])] =
      if (current.clientIdentifier.nonEmpty)
        Some((current.clientType, current.clientIdentifier, current.service, current.knownFact))
      else None
  }
}
