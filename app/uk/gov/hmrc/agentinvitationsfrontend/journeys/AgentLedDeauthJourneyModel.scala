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

package uk.gov.hmrc.agentinvitationsfrontend.journeys

import org.joda.time.LocalDate
import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult.{VatDetailsNotFound, VatKnownFactCheckOk, VatKnownFactNotMatched, VatRecordClientInsolvent, VatRecordMigrationInProgress}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, PptRef, Service, TrustTaxIdentifier, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentLedDeauthJourneyModel extends JourneyModel with Logging {

  sealed trait State

  override val root: State = State.SelectClientType

  object State {
    case object SelectClientType extends State

    case class SelectService(clientType: ClientType, enabledServices: Set[Service]) extends State

    case class IdentifyClient(clientType: ClientType, service: Service) extends State

    case class ConfirmPostcodeCgt(clientType: ClientType, cgtRef: CgtRef, postcode: Option[String], clientName: String) extends State
    case class ConfirmCountryCodeCgt(clientType: ClientType, cgtRef: CgtRef, countryCode: String, clientName: String) extends State

    case class ConfirmClient(clientType: ClientType, service: Service, clientName: Option[String], clientId: TaxIdentifier) extends State {
      require(Services.isSupported(clientType, service))
      require(service.supportedSuppliedClientIdType.clazz == clientId.getClass)
    }

    case class ConfirmCancel(service: Service, clientName: Option[String], clientId: String, isPartialAuth: Boolean = false) extends State
    case class AuthorisationCancelled(service: Service, clientName: Option[String], agencyName: String) extends State

    //error states
    trait ErrorState extends State
    case object KnownFactNotMatched extends ErrorState
    case class NotSignedUp(service: Service) extends State
    case class NotAuthorised(service: Service) extends State
    case class ResponseFailed(service: Service, clientName: Option[String], clientId: String) extends State
    case object TrustNotFound extends ErrorState
    case class CgtRefNotFound(cgtRef: CgtRef) extends ErrorState
    case class PptRefNotFound(pptRef: PptRef) extends ErrorState
  }

  object Transitions {

    import State._

    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type HasActiveRelationship = (Arn, String, Service) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, Service) => Future[Option[String]]
    type DOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]
    type VatRegDateMatches = (Vrn, LocalDate) => Future[VatKnownFactCheckResult]
    type DeleteRelationship = (Service, Arn, String) => Future[Option[Boolean]]
    type SetRelationshipEnded = (Arn, String, Service) => Future[Option[Boolean]]
    type GetAgencyName = Arn => Future[String]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type GetTrustName = TrustTaxIdentifier => Future[TrustResponse]

    def selectedClientType(agent: AuthorisedAgent)(clientTypeStr: String) = Transition {
      case SelectClientType =>
        val clientType = ClientType.toEnum(clientTypeStr)
        goto(SelectService(clientType, Services.supportedServicesFor(clientType)))
    }

    def chosenPersonalService(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(service: Service) = Transition {
      case SelectService(ClientType.Personal, enabledServices) =>
        if (enabledServices.contains(service)) {
          if (featureFlags.isServiceEnabled(service))
            goto(IdentifyClient(ClientType.Personal, service))
          else fail(new Exception(s"Service: ${service.id} feature flag is switched off"))
        } else goto(SelectService(ClientType.Personal, enabledServices))
    }

    def chosenBusinessServiceMulti(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(service: Service) =
      chosenBusinessService(featureFlags)(agent)(Some(service))

    def chosenBusinessService(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(mService: Option[Service]) = Transition {
      case SelectService(ClientType.Business, enabledServices) =>
        mService match {
          case Some(service) if enabledServices.contains(service) =>
            if (featureFlags.isServiceEnabled(service))
              goto(IdentifyClient(ClientType.Business, service))
            else fail(new Exception(s"Service: ${service.id} feature flag is switched off"))
          case _ => goto(root)
        }
    }

    def chosenTrustService(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(service: Service) =
      Transition {
        case SelectService(ClientType.Trust, enabledServices) =>
          if (enabledServices.contains(service) && featureFlags.isServiceEnabled(service))
            goto(IdentifyClient(ClientType.Trust, service))
          else fail(new Exception(s"Service: ${service.id} feature flag is switched off"))
        case _ => goto(root)
      }

    def submitIdentifyClientItsa(checkPostcodeMatches: CheckPostcodeMatches, getClientName: GetClientName)(agent: AuthorisedAgent)(
      itsaClient: ItsaClient): AgentLedDeauthJourneyModel.Transition = {

      def goToState(kfcResult: Option[Boolean]): Future[State] =
        for {
          finalState <- kfcResult match {
                         case Some(true) =>
                           getClientName(itsaClient.clientIdentifier, Service.MtdIt).flatMap(name =>
                             goto(ConfirmClient(ClientType.Personal, Service.MtdIt, name, Nino(itsaClient.clientIdentifier))))
                         case Some(false) => goto(KnownFactNotMatched)
                         case None        => goto(NotSignedUp(Service.MtdIt))
                       }
        } yield finalState

      Transition {
        case IdentifyClient(ClientType.Personal, Service.MtdIt) =>
          for {
            postcodeMatches <- if (itsaClient.postcode.nonEmpty)
                                checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode)
                              else throw new Exception("Postcode expected but none found")
            finalState <- goToState(postcodeMatches)
          } yield finalState
      }
    }

    def submitIdentifyClientIrv(
      checkDOBMatches: DOBMatches,
      getClientName: GetClientName,
      hasActiveRelationship: HasActiveRelationship,
      hasPartialAuthorisation: HasPartialAuthorisation)(agent: AuthorisedAgent)(irvClient: IrvClient): AgentLedDeauthJourneyModel.Transition = {

      def goToState(dobMatchResult: Option[Boolean]): Future[State] =
        for {
          finalState <- dobMatchResult match {
                         case Some(true) =>
                           getClientName(irvClient.clientIdentifier, Service.PersonalIncomeRecord).flatMap(
                             name =>
                               clientConfirmed(hasActiveRelationship)(hasPartialAuthorisation)(agent)(Confirmation(true))
                                 .apply(ConfirmClient(ClientType.Personal, Service.PersonalIncomeRecord, name, Nino(irvClient.clientIdentifier))))
                         case Some(false) => goto(KnownFactNotMatched)
                         case None        => goto(NotSignedUp(Service.PersonalIncomeRecord))
                       }
        } yield finalState

      Transition {
        case IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord) =>
          for {
            dobMatches <- if (irvClient.dob.nonEmpty)
                           checkDOBMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob))
                         else throw new Exception("Date of birth expected but none found")
            finalState <- goToState(dobMatches)
          } yield finalState
      }
    }

    def submitIdentifyClientTrust(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyClient(ClientType.Trust, Service.Trust | Service.TrustNT) =>
          getTrustName(trustClient.taxId).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) =>
                trustClient.taxId match {
                  case Utr(_) => goto(ConfirmClient(ClientType.Trust, Service.Trust, Some(name), Utr(trustClient.taxId.value)))
                  case Urn(_) => goto(ConfirmClient(ClientType.Trust, Service.TrustNT, Some(name), Urn(trustClient.taxId.value)))
                }
              case Left(invalidTrust) =>
                logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.taxId}")
                goto(TrustNotFound)
            }
          }
      }

    def submitIdentifyClientCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      cgtClient: CgtClient): AgentLedDeauthJourneyModel.Transition = {
      def handle(showPostcode: CgtSubscription => State, showCountryCode: CgtSubscription => State) =
        getCgtSubscription(cgtClient.cgtRef).map {
          case Some(subscription) =>
            if (subscription.isUKBasedClient) {
              showPostcode(subscription)
            } else {
              showCountryCode(subscription)
            }
          case None =>
            CgtRefNotFound(cgtClient.cgtRef)
        }

      Transition {
        case IdentifyClient(clientType, Service.CapitalGains) =>
          handle(
            cgtSubscription => ConfirmPostcodeCgt(clientType, cgtClient.cgtRef, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription =>
              ConfirmCountryCodeCgt(
                clientType,
                cgtClient.cgtRef,
                cgtSubscription.countryCode,
                cgtSubscription.name
            )
          )
      }
    }

    def submitIdentifyClientPpt(getPptSubscription: GetPptSubscription)(agent: AuthorisedAgent)(
      pptClient: PptClient): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case IdentifyClient(clientType, Service.Ppt) =>
          getPptSubscription(pptClient.pptRef).map {
            case Some(subscription) =>
              ConfirmClient(clientType, Service.Ppt, Some(subscription.customerName), pptClient.pptRef)
            case None =>
              PptRefNotFound(pptClient.pptRef)
          }
      }

    def confirmPostcodeCgt(agent: AuthorisedAgent)(postcode: Postcode): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case ConfirmPostcodeCgt(clientType, cgtRef, postcodeFromDes, name) =>
          if (postcodeFromDes.contains(postcode.value)) {
            goto(ConfirmClient(clientType, Service.CapitalGains, Some(name), cgtRef))
          } else {
            goto(KnownFactNotMatched)
          }
      }

    def confirmCountryCodeCgt(agent: AuthorisedAgent)(countryCode: CountryCode): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case ConfirmCountryCodeCgt(clientType, cgtRef, countryCodeFromDes, name) =>
          if (countryCodeFromDes.contains(countryCode.value)) {
            goto(ConfirmClient(clientType, Service.CapitalGains, Some(name), cgtRef))
          } else {
            goto(KnownFactNotMatched)
          }
      }

    def submitIdentifyClientVat(vatRegDateMatches: VatRegDateMatches, getClientName: GetClientName)(agent: AuthorisedAgent)(
      vatClient: VatClient): AgentLedDeauthJourneyModel.Transition = {

      def vatRegDateMatchResult: Future[VatKnownFactCheckResult] =
        if (vatClient.registrationDate.nonEmpty)
          vatRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
        else throw new Exception("Vat registration date expected but none found")

      def goToState(vatRegDateMatchResult: VatKnownFactCheckResult, finalState: Option[String] => State): Future[State] =
        for {
          finalState <- vatRegDateMatchResult match {
                         case VatKnownFactCheckOk | VatRecordClientInsolvent =>
                           getClientName(vatClient.clientIdentifier, Service.Vat).flatMap(name => goto(finalState(name)))
                         case VatKnownFactNotMatched       => goto(KnownFactNotMatched)
                         case VatRecordMigrationInProgress => goto(NotSignedUp(Service.Vat)) //for now until we have content
                         case VatDetailsNotFound           => goto(NotSignedUp(Service.Vat))
                       }
        } yield finalState

      def checkVatRegDateAndGoToState(finalState: Option[String] => State): Future[State] =
        for {
          hasVatRegDate <- vatRegDateMatchResult
          finalState    <- goToState(hasVatRegDate, finalState)
        } yield finalState

      Transition {
        case IdentifyClient(ClientType.Personal, Service.Vat) =>
          def nameToState(name: Option[String]) =
            ConfirmClient(ClientType.Personal, Service.Vat, name, Vrn(vatClient.clientIdentifier))

          checkVatRegDateAndGoToState(nameToState)

        case IdentifyClient(ClientType.Business, Service.Vat) =>
          def nameToState(name: Option[String]) =
            ConfirmClient(ClientType.Business, Service.Vat, name, Vrn(vatClient.clientIdentifier))

          checkVatRegDateAndGoToState(nameToState)
      }
    }

    def clientConfirmed(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(agent: AuthorisedAgent)(
      confirmation: Confirmation): AgentLedDeauthJourneyModel.Transition = {

      def gotoFinalState(clientId: String, service: Service, name: Option[String]) =
        if (confirmation.choice) {
          for {
            relationshipIsActive <- hasActiveRelationship(agent.arn, clientId, service)
            result <- if (relationshipIsActive) goto(ConfirmCancel(service, name, clientId))
                     else if (service == Service.MtdIt) hasPartialAuthorisation(agent.arn, clientId).flatMap {
                       case true  => goto(ConfirmCancel(service, name, clientId, true))
                       case false => goto(NotAuthorised(service))
                     } else goto(NotAuthorised(service))
          } yield result
        } else goto(root)

      Transition {
        case ConfirmClient(clientType, service, name, taxId) => gotoFinalState(taxId.value, service, name)
      }
    }

    def cancelConfirmed(deleteRelationship: DeleteRelationship, getAgencyName: GetAgencyName, setRelationshipEnded: SetRelationshipEnded)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmCancel(service, clientName, clientId, isAltItsa) =>
          if (confirmation.choice) {
            val deAuthResult = if (isAltItsa) setRelationshipEnded(agent.arn, clientId, service) else deleteRelationship(service, agent.arn, clientId)
            deAuthResult.flatMap {
              case Some(true) =>
                for {
                  name   <- getAgencyName(agent.arn)
                  result <- goto(AuthorisationCancelled(service, clientName, name))
                } yield result
              case _ => goto(ResponseFailed(service, clientName, clientId))
            }
          } else goto(root)

        case ResponseFailed(service, clientName, clientId) =>
          deleteRelationship(service, agent.arn, clientId).flatMap {
            case Some(true) =>
              getAgencyName(agent.arn).flatMap(name => goto(AuthorisationCancelled(service, clientName, name)))
            case _ => goto(ResponseFailed(service, clientName, clientId))
          }
      }
  }
}
