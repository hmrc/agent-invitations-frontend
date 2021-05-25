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

package uk.gov.hmrc.agentinvitationsfrontend.journeys

import org.joda.time.LocalDate
import play.api.Logging
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.Transitions.{GetCgtSubscription, GetTrustName}
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, CgtRef, Urn, Utr, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentLedDeauthJourneyModel extends JourneyModel with Logging {

  sealed trait State

  override val root: State = State.SelectClientType

  object State {
    case object SelectClientType extends State
    case class SelectServicePersonal(enabledServices: Set[String]) extends State
    case object SelectServiceBusiness extends State
    case class SelectServiceTrust(enabledServices: Set[String]) extends State
    case class IdentifyClientPersonal(service: String) extends State
    case object IdentifyClientBusiness extends State
    case object IdentifyClientTrust extends State
    case object IdentifyClientCgt extends State

    case class ConfirmPostcodeCgt(cgtRef: CgtRef, postcode: Option[String], clientName: String) extends State
    case class ConfirmCountryCodeCgt(cgtRef: CgtRef, countryCode: String, clientName: String) extends State

    case class ConfirmClientCgt(cgtRef: CgtRef, clientName: String) extends State

    case class ConfirmClientItsa(clientName: Option[String], nino: Nino) extends State
    case class ConfirmClientIrv(clientName: Option[String], nino: Nino) extends State
    case class ConfirmClientPersonalVat(clientName: Option[String], vrn: Vrn) extends State
    case class ConfirmClientBusiness(clientName: Option[String], vrn: Vrn) extends State
    case class ConfirmClientTrust(clientName: String, utr: Utr) extends State
    case class ConfirmClientTrustNT(clientName: String, urn: Urn) extends State
    case class ConfirmCancel(service: String, clientName: Option[String], clientId: String, isPartialAuth: Boolean = false) extends State
    case class AuthorisationCancelled(service: String, clientName: Option[String], agencyName: String) extends State

    //error states
    case object KnownFactNotMatched extends State
    case class NotSignedUp(service: String) extends State
    case class NotAuthorised(service: String) extends State
    case class ResponseFailed(service: String, clientName: Option[String], clientId: String) extends State
    case object TrustNotFound extends State
    case class CgtRefNotFound(cgtRef: CgtRef) extends State
  }

  object Transitions {

    import State._

    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type DOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]
    type VatRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type DeleteRelationship = (String, Arn, String) => Future[Option[Boolean]]
    type SetRelationshipEnded = (Arn, String, String) => Future[Option[Boolean]]
    type GetAgencyName = Arn => Future[String]

    def selectedClientType(agent: AuthorisedAgent)(clientType: String) = Transition {
      case SelectClientType =>
        clientType match {
          case "personal" =>
            val enabledPersonalServices =
              if (agent.isWhitelisted)
                Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)
              else
                Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD)
            goto(SelectServicePersonal(enabledPersonalServices))
          case "business" => goto(SelectServiceBusiness)
          case "trust"    => goto(SelectServiceTrust(agent.trustServices))
        }
    }

    def chosenPersonalService(showItsaFlag: Boolean, showPirFlag: Boolean, showVatFlag: Boolean, showCgtFlag: Boolean)(agent: AuthorisedAgent)(
      service: String) = Transition {
      case SelectServicePersonal(enabledServices) =>
        if (enabledServices.contains(service)) {
          service match {
            case HMRCMTDIT =>
              if (showItsaFlag) goto(IdentifyClientPersonal(service))
              else fail(new Exception(s"Service: $service feature flag is switched off"))

            case HMRCPIR =>
              if (showPirFlag) goto(IdentifyClientPersonal(service))
              else fail(new Exception(s"Service: $service feature flag is switched off"))

            case HMRCMTDVAT =>
              if (showVatFlag) goto(IdentifyClientPersonal(service))
              else fail(new Exception(s"Service: $service feature flag is switched off"))

            case HMRCCGTPD =>
              if (showCgtFlag) goto(IdentifyClientPersonal(service))
              else fail(new Exception(s"Service: $service feature flag is switched off"))
          }
        } else goto(SelectServicePersonal(enabledServices))
    }

    def chosenBusinessService(showVatFlag: Boolean)(agent: AuthorisedAgent)(service: String) = Transition {
      case SelectServiceBusiness if service.nonEmpty =>
        if (showVatFlag) goto(IdentifyClientBusiness)
        else fail(new Exception(s"Service: $HMRCMTDVAT feature flag is switched off"))

      case SelectServiceBusiness => goto(root)
    }

    def chosenTrustService(showTrustFlag: Boolean, showCgtFlag: Boolean)(agent: AuthorisedAgent)(service: String) =
      Transition {
        case SelectServiceTrust(enabledServices) =>
          if (enabledServices.contains(service)) {
            service match {
              case TRUST =>
                if (showTrustFlag) goto(IdentifyClientTrust)
                else fail(new Exception(s"Service: $service feature flag is switched off"))

              case HMRCCGTPD =>
                if (showCgtFlag) goto(IdentifyClientCgt)
                else fail(new Exception(s"Service: $service feature flag is switched off"))
            }
          } else fail(new Exception(s"Service: $service feature flag is switched off"))

        case _ => goto(root)
      }

    def submitIdentifyClientItsa(
      checkPostcodeMatches: CheckPostcodeMatches,
      getClientName: GetClientName,
      hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(itsaClient: ItsaClient): AgentLedDeauthJourneyModel.Transition = {

      def goToState(kfcResult: Option[Boolean]): Future[State] =
        for {
          finalState <- kfcResult match {
                         case Some(true) =>
                           getClientName(itsaClient.clientIdentifier, HMRCMTDIT).flatMap(name =>
                             goto(ConfirmClientItsa(name, Nino(itsaClient.clientIdentifier))))
                         case Some(false) => goto(KnownFactNotMatched)
                         case None        => goto(NotSignedUp(HMRCMTDIT))
                       }
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCMTDIT) =>
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
                           getClientName(irvClient.clientIdentifier, HMRCPIR).flatMap(
                             name =>
                               clientConfirmed(hasActiveRelationship)(hasPartialAuthorisation)(agent)(Confirmation(true))
                                 .apply(ConfirmClientIrv(name, Nino(irvClient.clientIdentifier))))
                         case Some(false) => goto(KnownFactNotMatched)
                         case None        => goto(NotSignedUp(HMRCPIR))
                       }
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCPIR) =>
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
        case IdentifyClientTrust =>
          getTrustName(trustClient.taxId).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) => {
                trustClient.taxId match {
                  case Utr(_) => goto(ConfirmClientTrust(name, Utr(trustClient.taxId.value)))
                  case Urn(_) => goto(ConfirmClientTrustNT(name, Urn(trustClient.taxId.value)))
                }

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
        case IdentifyClientPersonal(HMRCCGTPD) | IdentifyClientCgt =>
          handle(
            cgtSubscription => ConfirmPostcodeCgt(cgtClient.cgtRef, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription =>
              ConfirmCountryCodeCgt(
                cgtClient.cgtRef,
                cgtSubscription.countryCode,
                cgtSubscription.name
            )
          )
      }
    }

    def confirmPostcodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      postcode: Postcode): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case ConfirmPostcodeCgt(cgtRef, postcodeFromDes, name) =>
          if (postcodeFromDes.contains(postcode.value)) {
            goto(ConfirmClientCgt(cgtRef, name))
          } else {
            goto(KnownFactNotMatched)
          }
      }

    def confirmCountryCodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      countryCode: CountryCode): AgentLedDeauthJourneyModel.Transition =
      Transition {
        case ConfirmCountryCodeCgt(cgtRef, countryCodeFromDes, name) =>
          if (countryCodeFromDes.contains(countryCode.value)) {
            goto(ConfirmClientCgt(cgtRef, name))
          } else {
            goto(KnownFactNotMatched)
          }
      }

    def submitIdentifyClientVat(vatRegDateMatches: VatRegDateMatches, getClientName: GetClientName, hasActiveRelationship: HasActiveRelationship)(
      agent: AuthorisedAgent)(vatClient: VatClient): AgentLedDeauthJourneyModel.Transition = {

      def vatRegDateMatchResult: Future[Option[Int]] =
        if (vatClient.registrationDate.nonEmpty)
          vatRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
        else throw new Exception("Vat registration date expected but none found")

      def goToState(vatRegDateMatchResult: Option[Int], finalState: Option[String] => State): Future[State] =
        for {
          finalState <- vatRegDateMatchResult match {
                         case Some(204) =>
                           getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap(name => goto(finalState(name)))
                         case Some(403) => goto(KnownFactNotMatched)
                         case _         => goto(NotSignedUp(HMRCMTDVAT))
                       }
        } yield finalState

      def checkVatRegDateAndGoToState(finalState: Option[String] => State): Future[State] =
        for {
          hasVatRegDate <- vatRegDateMatchResult
          finalState    <- goToState(hasVatRegDate, finalState)
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCMTDVAT) =>
          def nameToState(name: Option[String]) =
            ConfirmClientPersonalVat(name, Vrn(vatClient.clientIdentifier))

          checkVatRegDateAndGoToState(nameToState)

        case IdentifyClientBusiness =>
          def nameToState(name: Option[String]) =
            ConfirmClientBusiness(name, Vrn(vatClient.clientIdentifier))
          checkVatRegDateAndGoToState(nameToState)
      }
    }

    def clientConfirmed(hasActiveRelationship: HasActiveRelationship)(hasPartialAuthorisation: HasPartialAuthorisation)(agent: AuthorisedAgent)(
      confirmation: Confirmation): AgentLedDeauthJourneyModel.Transition = {

      def gotoFinalState(clientId: String, service: String, name: Option[String]) =
        if (confirmation.choice) {
          for {
            relationshipIsActive <- hasActiveRelationship(agent.arn, clientId, service)
            result <- if (relationshipIsActive) goto(ConfirmCancel(service, name, clientId))
                     else if (service == HMRCMTDIT) hasPartialAuthorisation(agent.arn, clientId).flatMap {
                       case true  => goto(ConfirmCancel(service, name, clientId, true))
                       case false => goto(NotAuthorised(service))
                     } else goto(NotAuthorised(service))
          } yield result
        } else goto(root)

      Transition {
        case ConfirmClientItsa(name, nino)       => gotoFinalState(nino.value, HMRCMTDIT, name)
        case ConfirmClientIrv(name, nino)        => gotoFinalState(nino.value, HMRCPIR, name)
        case ConfirmClientPersonalVat(name, vrn) => gotoFinalState(vrn.value, HMRCMTDVAT, name)
        case ConfirmClientBusiness(name, vrn)    => gotoFinalState(vrn.value, HMRCMTDVAT, name)
        case ConfirmClientTrust(name, utr)       => gotoFinalState(utr.value, TAXABLETRUST, Some(name))
        case ConfirmClientTrustNT(name, urn)     => gotoFinalState(urn.value, NONTAXABLETRUST, Some(name))
        case ConfirmClientCgt(cgtRef, name)      => gotoFinalState(cgtRef.value, HMRCCGTPD, Some(name))
      }
    }

    def cancelConfirmed(deleteRelationship: DeleteRelationship, getAgencyName: GetAgencyName, setRelationshipEnded: SetRelationshipEnded)(
      agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case ConfirmCancel(service, clientName, clientId, isAltItsa) =>
          if (confirmation.choice) {
            val fCall = if (!isAltItsa) deleteRelationship(service, agent.arn, clientId) else Future successful Some(true)
            fCall.flatMap {
              case Some(true) => {
                for {
                  name    <- getAgencyName(agent.arn)
                  updated <- setRelationshipEnded(agent.arn, clientId, service)
                  result <- if (updated.exists(x => x)) goto(AuthorisationCancelled(service, clientName, name))
                           else {
                             logger.warn(s"set relationship ended failed...proceeding with authorisation cancelled")
                             goto(AuthorisationCancelled(service, clientName, name))
                           }
                } yield result
              }
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
