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
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.TaxIdentifier
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.{ExecutionContext, Future}

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
    type HasActiveRelationship = (Arn, String, Service) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, Service) => Future[Option[String]]
    type DeleteRelationship = (Service, Arn, String) => Future[Option[Boolean]]
    type SetRelationshipEnded = (Arn, String, Service) => Future[Option[Boolean]]
    type GetAgencyName = Arn => Future[String]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type GetCbcSubscription = CbcId => Future[Option[SimpleCbcSubscription]]
    type CheckKnownFact = ClientIdSet => Future[KnownFactResult]
  }

  import TransitionEffects._

  case class Transitions(
    featureFlags: FeatureFlags,
    hasActiveRelationshipFor: HasActiveRelationship,
    hasPartialAuthorisationFor: HasPartialAuthorisation,
    getClientName: GetClientName,
    deleteRelationship: DeleteRelationship,
    setRelationshipEnded: SetRelationshipEnded,
    getAgencyName: GetAgencyName,
    getCgtSubscription: GetCgtSubscription,
    getPptSubscription: GetPptSubscription,
    getCbcSubscription: GetCbcSubscription,
    checkKnownFact: CheckKnownFact
  )(implicit ec: ExecutionContext) {

    import State._

    def selectedClientType(agent: AuthorisedAgent)(clientTypeStr: String) = Transition {
      case SelectClientType =>
        val clientType = ClientType.toEnum(clientTypeStr)
        val availableServices = featureFlags.enabledServicesFor(clientType)
        goto(SelectService(clientType, availableServices))
    }

    def chosenServiceMulti(agent: AuthorisedAgent)(service: Service) =
      chosenService(agent)(Some(service))

    def chosenService(agent: AuthorisedAgent)(mService: Option[Service]) =
      Transition {
        case SelectService(clientType, _) =>
          mService match {
            case Some(service) =>
              if (featureFlags.isServiceEnabled(service))
                goto(IdentifyClient(clientType, service))
              else fail(new Exception(s"Service: ${service.id} is not enabled"))
            case _ => goto(root)
          }
        case _ => goto(root)
      }

    def submitIdentifyClientItsa(agent: AuthorisedAgent)(itsaClient: ItsaClient): AgentLedDeauthJourneyModel.Transition = {

      def goToState(kfcResult: KnownFactResult): Future[State] =
        for {
          finalState <- kfcResult match {
                         case Pass =>
                           getClientName(itsaClient.nino.value, Service.MtdIt).flatMap(name =>
                             goto(ConfirmClient(ClientType.Personal, Service.MtdIt, name, itsaClient.nino)))
                         case Fail(NotFound)       => goto(NotSignedUp(Service.MtdIt))
                         case Fail(NotMatched) | _ => goto(KnownFactNotMatched)
                       }
        } yield finalState

      Transition {
        case IdentifyClient(ClientType.Personal, Service.MtdIt) =>
          for {
            postcodeMatches <- if (itsaClient.postcode.nonEmpty) checkKnownFact(itsaClient)
                              else throw new Exception("Postcode expected but none found")
            finalState <- goToState(postcodeMatches)
          } yield finalState
      }
    }

    def submitIdentifyClientIrv(agent: AuthorisedAgent)(irvClient: IrvClient): AgentLedDeauthJourneyModel.Transition = {

      def goToState(knownFactResult: KnownFactResult): Future[State] =
        for {
          finalState <- knownFactResult match {
                         case Pass =>
                           getClientName(irvClient.nino.value, Service.PersonalIncomeRecord).flatMap(
                             name =>
                               clientConfirmed(agent)(Confirmation(true))
                                 .apply(ConfirmClient(ClientType.Personal, Service.PersonalIncomeRecord, name, irvClient.nino)))
                         case Fail(NotFound)       => goto(NotSignedUp(Service.PersonalIncomeRecord))
                         case Fail(NotMatched) | _ => goto(KnownFactNotMatched)
                       }
        } yield finalState

      Transition {
        case IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord) =>
          for {
            kfcResult <- if (irvClient.dob.nonEmpty) checkKnownFact(irvClient)
                        else throw new Exception("Date of birth expected but none found")
            finalState <- goToState(kfcResult)
          } yield finalState
      }
    }

    def submitIdentifyClientTrust(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyClient(ClientType.Trust, Service.Trust | Service.TrustNT) =>
          getClientName(trustClient.taxId.value, Service.Trust).flatMap {
            case Some(name) =>
              trustClient.taxId match {
                case Utr(_) => goto(ConfirmClient(ClientType.Trust, Service.Trust, Some(name), Utr(trustClient.taxId.value)))
                case Urn(_) => goto(ConfirmClient(ClientType.Trust, Service.TrustNT, Some(name), Urn(trustClient.taxId.value)))
              }
            case None => goto(TrustNotFound)
          }
      }

    def submitIdentifyClientCgt(agent: AuthorisedAgent)(cgtRef: CgtRef): AgentLedDeauthJourneyModel.Transition = Transition {
      case IdentifyClient(clientType, Service.CapitalGains) =>
        getCgtSubscription(cgtRef).map {
          case Some(subscription) if subscription.isUKBasedClient =>
            ConfirmPostcodeCgt(clientType, cgtRef, subscription.postCode, subscription.name)
          case Some(subscription) if !subscription.isUKBasedClient =>
            ConfirmCountryCodeCgt(clientType, cgtRef, subscription.countryCode, subscription.name)
          case None =>
            CgtRefNotFound(cgtRef)
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

    def submitIdentifyClientCbc(agent: AuthorisedAgent)(cbcClient: CbcClient): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case state @ IdentifyClient(clientType, Service.Cbc | Service.CbcNonUk) =>
          for {
            knownFactResult <- checkKnownFact(cbcClient)
            nextState <- if (knownFactResult.isOk) {
                          for {
                            maybeSubscription <- getCbcSubscription(cbcClient.cbcId)
                            subscription = maybeSubscription.getOrElse(
                              throw new RuntimeException(s"CBC subscription for ${cbcClient.cbcId} not found!"))
                            adjustedService = if (subscription.isGBUser) Service.Cbc else Service.CbcNonUk
                            clientName = subscription.anyAvailableName.getOrElse(cbcClient.cbcId.value)
                          } yield ConfirmClient(clientType, adjustedService, Some(clientName), cbcClient.cbcId)
                        } else Future.successful(KnownFactNotMatched)
          } yield nextState
      }

    def submitIdentifyClientPillar2(agent: AuthorisedAgent)(pillar2Client: Pillar2Client): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case IdentifyClient(clientType, Service.Pillar2) =>
          for {
            knownFactResult <- checkKnownFact(pillar2Client)
            nextState <- if (knownFactResult.isOk) {
                          for {
                            mClientName <- getClientName(pillar2Client.plrId.value, Service.Pillar2)
                            clientName = mClientName.getOrElse(pillar2Client.plrId.value)
                          } yield ConfirmClient(clientType, Service.Pillar2, Some(clientName), pillar2Client.plrId)
                        } else Future.successful(KnownFactNotMatched)
          } yield nextState
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

      def vatRegDateMatchResult: Future[KnownFactResult] =
        if (vatClient.registrationDate.nonEmpty) checkKnownFact(vatClient)
        else throw new Exception("Vat registration date expected but none found")

      def goToState(vatRegDateMatchResult: KnownFactResult, finalState: Option[String] => State): Future[State] =
        for {
          finalState <- vatRegDateMatchResult match {
                         case Pass | Fail(VatClientInsolvent) =>
                           getClientName(vatClient.vrn.value, Service.Vat).flatMap(name => goto(finalState(name)))
                         case Fail(VatMigrationInProgress) =>
                           goto(NotSignedUp(Service.Vat)) //for now until we have content
                         case Fail(NotFound)       => goto(NotSignedUp(Service.Vat))
                         case Fail(NotMatched) | _ => goto(KnownFactNotMatched)
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
            ConfirmClient(ClientType.Personal, Service.Vat, name, vatClient.vrn)

          checkVatRegDateAndGoToState(nameToState)

        case IdentifyClient(ClientType.Business, Service.Vat) =>
          def nameToState(name: Option[String]) =
            ConfirmClient(ClientType.Business, Service.Vat, name, vatClient.vrn)

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
