/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.play.fsm.JourneyModel

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentLedDeauthJourneyModel extends JourneyModel with Logging {

  sealed trait State

  override val root: State = State.SelectClientType

  object State {
    case object SelectClientType extends State

    case class SelectService(clientType: ClientType, availableServices: Set[Service]) extends State

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

  object TransitionEffects {
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type HasActiveRelationship = (Arn, String, Service) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, Service) => Future[Option[String]]
    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]
    type VatRegDateMatches = (Vrn, LocalDate) => Future[VatKnownFactCheckResult]
    type DeleteRelationship = (Service, Arn, String) => Future[Option[Boolean]]
    type SetRelationshipEnded = (Arn, String, Service) => Future[Option[Boolean]]
    type GetAgencyName = Arn => Future[String]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type GetTrustName = TrustTaxIdentifier => Future[TrustResponse]
  }

  import TransitionEffects._

  case class Transitions(
    featureFlags: FeatureFlags,
    checkPostcodeMatches: CheckPostcodeMatches,
    hasActiveRelationshipFor: HasActiveRelationship,
    hasPartialAuthorisationFor: HasPartialAuthorisation,
    getClientName: GetClientName,
    checkDOBMatches: CheckDOBMatches,
    checkVatRegDateMatches: VatRegDateMatches,
    deleteRelationship: DeleteRelationship,
    setRelationshipEnded: SetRelationshipEnded,
    getAgencyName: GetAgencyName,
    getCgtSubscription: GetCgtSubscription,
    getPptSubscription: GetPptSubscription,
    getTrustName: GetTrustName
  ) {

    import State._

    def selectedClientType(agent: AuthorisedAgent)(clientTypeStr: String) = Transition {
      case SelectClientType =>
        val clientType = ClientType.toEnum(clientTypeStr)
        val availableServices = featureFlags.enabledServicesFor(clientType, Some(agent))
        goto(SelectService(clientType, availableServices))
    }

    def chosenServiceMulti(agent: AuthorisedAgent)(service: Service) =
      chosenService(agent)(Some(service))

    def chosenService(agent: AuthorisedAgent)(mService: Option[Service]) =
      Transition {
        case SelectService(clientType, _) =>
          mService match {
            case Some(service) =>
              if (featureFlags.isServiceEnabled(service, Some(agent)))
                goto(IdentifyClient(clientType, service))
              else fail(new Exception(s"Service: ${service.id} is not enabled"))
            case _ => goto(root)
          }
        case _ => goto(root)
      }

    def submitIdentifyClientItsa(agent: AuthorisedAgent)(itsaClient: ItsaClient): AgentLedDeauthJourneyModel.Transition = {

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

    def submitIdentifyClientIrv(agent: AuthorisedAgent)(irvClient: IrvClient): AgentLedDeauthJourneyModel.Transition = {

      def goToState(dobMatchResult: Option[Boolean]): Future[State] =
        for {
          finalState <- dobMatchResult match {
                         case Some(true) =>
                           getClientName(irvClient.clientIdentifier, Service.PersonalIncomeRecord).flatMap(
                             name =>
                               clientConfirmed(agent)(Confirmation(true))
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

    def submitIdentifyClientTrust(agent: AuthorisedAgent)(trustClient: TrustClient) =
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

    def submitIdentifyClientCgt(agent: AuthorisedAgent)(cgtClient: CgtClient): AgentLedDeauthJourneyModel.Transition = Transition {
      case IdentifyClient(clientType, Service.CapitalGains) =>
        getCgtSubscription(cgtClient.cgtRef).map {
          case Some(subscription) if subscription.isUKBasedClient =>
            ConfirmPostcodeCgt(clientType, cgtClient.cgtRef, subscription.postCode, subscription.name)
          case Some(subscription) if !subscription.isUKBasedClient =>
            ConfirmCountryCodeCgt(clientType, cgtClient.cgtRef, subscription.countryCode, subscription.name)
          case None =>
            CgtRefNotFound(cgtClient.cgtRef)
        }
    }

    def submitIdentifyClientPpt(agent: AuthorisedAgent)(pptClient: PptClient): AgentLedDeauthJourneyModel.Transition =
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

    def submitIdentifyClientVat(agent: AuthorisedAgent)(vatClient: VatClient): AgentLedDeauthJourneyModel.Transition = {

      def vatRegDateMatchResult: Future[VatKnownFactCheckResult] =
        if (vatClient.registrationDate.nonEmpty)
          checkVatRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
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

    def clientConfirmed(agent: AuthorisedAgent)(confirmation: Confirmation): AgentLedDeauthJourneyModel.Transition = {

      def gotoFinalState(clientId: String, service: Service, name: Option[String]) =
        if (confirmation.choice) {
          for {
            relationshipIsActive <- hasActiveRelationshipFor(agent.arn, clientId, service)
            result <- if (relationshipIsActive) goto(ConfirmCancel(service, name, clientId))
                     else if (service == Service.MtdIt) hasPartialAuthorisationFor(agent.arn, clientId).flatMap {
                       case true  => goto(ConfirmCancel(service, name, clientId, true))
                       case false => goto(NotAuthorised(service))
                     } else goto(NotAuthorised(service))
          } yield result
        } else goto(root)

      Transition {
        case ConfirmClient(clientType, service, name, taxId) => gotoFinalState(taxId.value, service, name)
      }
    }

    def cancelConfirmed(agent: AuthorisedAgent)(confirmation: Confirmation) =
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
