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

package uk.gov.hmrc.agentinvitationsfrontend.journeys

import org.joda.time.LocalDate
import play.api.Logger
import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AgentLedDeauthJourneyModel extends JourneyModel {

  sealed trait State

  override val root: State = State.SelectClientType

  object State {
    case object SelectClientType extends State
    case class SelectServicePersonal(enabledServices: Set[String]) extends State
    case object SelectServiceBusiness extends State
    case class IdentifyClientPersonal(service: String) extends State
    case object IdentifyClientBusiness extends State
    case class ConfirmClientItsa(clientName: Option[String], nino: Nino) extends State
    case class ConfirmClientIrv(clientName: Option[String], nino: Nino) extends State
    case class ConfirmClientPersonalVat(clientName: Option[String], vrn: Vrn) extends State
    case class ConfirmClientBusiness(clientName: Option[String], vrn: Vrn) extends State
    case class ConfirmCancel(service: String, clientName: Option[String], clientId: String) extends State
    case class AuthorisationCancelled(service: String, clientName: Option[String], agencyName: String) extends State

    //error states
    case object KnownFactNotMatched extends State
    case class NotSignedUp(service: String) extends State
    case object CannotCreateRequest extends State
    case class NotAuthorised(service: String) extends State
    case object ResponseFailed extends State
  }

  object Transitions {

    import ClientType._
    import State._

    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]]
    type DOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]
    type VatRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]
    type DeleteRelationship = (String, Arn, String) => Future[Option[Boolean]]
    type GetAgencyName = Arn => Future[String]

    def chosenClientType(agent: AuthorisedAgent)(clientType: ClientType) = Transition {
      case SelectClientType if clientType == personal =>
        val enabledPersonalServices =
          if (agent.isWhitelisted)
            Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
          else
            Set(HMRCMTDIT, HMRCMTDVAT)
        goto(SelectServicePersonal(enabledPersonalServices))

      case SelectClientType if clientType == business => goto(SelectServiceBusiness)
    }

    def chosenPersonalService(showItsaFlag: Boolean, showPirFlag: Boolean, showVatFlag: Boolean)(
      agent: AuthorisedAgent)(service: String) = Transition {
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
          }
        } else goto(SelectServicePersonal(enabledServices))
    }

    def chosenBusinessService(showVatFlag: Boolean)(agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case SelectServiceBusiness if confirmation.choice =>
        if (showVatFlag) goto(IdentifyClientBusiness)
        else fail(new Exception(s"Service: $HMRCMTDVAT feature flag is switched off"))

      case SelectServiceBusiness => goto(root)
    }

    def submitIdentifyClientItsa(
      checkPostcodeMatches: CheckPostcodeMatches,
      getClientName: GetClientName,
      hasActiveRelationship: HasActiveRelationship)(showKFCItsa: Boolean, redirectToConfirmItsa: Boolean)(
      agent: AuthorisedAgent)(itsaClient: ItsaClient): AgentLedDeauthJourneyModel.Transition = {

      def goToFinalState(finalState: Option[String] => State): Future[State] =
        for {
          postcodeMatches <- itsaClient.postcode match {
                              case _ if !showKFCItsa => Future successful Some(true)
                              case Some(postcode)    => checkPostcodeMatches(Nino(itsaClient.clientIdentifier), postcode)
                              case None              => throw new Exception("Postcode expected but none found")
                            }
          finalState <- postcodeMatches match {
                         case Some(true) =>
                           getClientName(itsaClient.clientIdentifier, HMRCMTDIT).flatMap(name => goto(finalState(name)))
                         case Some(false) => goto(KnownFactNotMatched)
                         case None        => goto(NotSignedUp(HMRCMTDIT))
                       }
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCMTDIT) =>
          def nameToState(name: Option[String]) =
            ConfirmClientItsa(name, Nino(itsaClient.clientIdentifier))

          if (redirectToConfirmItsa) goToFinalState(nameToState)
          else
            goToFinalState(nameToState).flatMap { finalState =>
              clientConfirmed(hasActiveRelationship)(agent)(Confirmation(true)).apply(finalState)
            }
      }
    }

    def submitIdentifyClientIrv(
      checkDOBMatches: DOBMatches,
      getClientName: GetClientName,
      hasActiveRelationship: HasActiveRelationship)(showKFCIrv: Boolean, redirectToConfirmIrv: Boolean)(
      agent: AuthorisedAgent)(irvClient: IrvClient): AgentLedDeauthJourneyModel.Transition = {

      def goToFinalState(finalState: Option[String] => State): Future[State] =
        for {
          dobMatches <- irvClient.dob match {
                         case _ if !showKFCIrv => Future successful Some(true)
                         case Some(dob) =>
                           checkDOBMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(dob))
                         case None => throw new Exception(s"Date of birth expected but none found")
                       }
          finalState <- dobMatches match {
                         case Some(true) =>
                           getClientName(irvClient.clientIdentifier, HMRCPIR).flatMap(name => goto(finalState(name)))
                         case Some(false) => goto(KnownFactNotMatched)
                         case None        => goto(NotSignedUp(HMRCPIR))
                       }
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCPIR) =>
          def nameToState(name: Option[String]) =
            ConfirmClientIrv(name, Nino(irvClient.clientIdentifier))

          if (redirectToConfirmIrv) goToFinalState(nameToState)
          else
            goToFinalState(nameToState).flatMap { finalState =>
              clientConfirmed(hasActiveRelationship)(agent)(Confirmation(true)).apply(finalState)
            }
      }
    }

    def submitIdentifyClientVat(
      vatRegDateMatches: VatRegDateMatches,
      getClientName: GetClientName,
      hasActiveRelationship: HasActiveRelationship)(showKFCVat: Boolean, redirectToConfirmVat: Boolean)(
      agent: AuthorisedAgent)(vatClient: VatClient): AgentLedDeauthJourneyModel.Transition = {
      def goToFinalState(finalState: Option[String] => State): Future[State] =
        for {
          hasVatRegDate <- vatClient.registrationDate match {
                            case _ if !showKFCVat => Future successful Some(204)
                            case Some(vatRegDate) =>
                              vatRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatRegDate))
                            case None => throw new Exception(s"Vat registration date expected but none found")
                          }
          finalState <- hasVatRegDate match {
                         case Some(204) =>
                           getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap(name => goto(finalState(name)))
                         case Some(403) => goto(KnownFactNotMatched)
                         case Some(423) => goto(CannotCreateRequest)
                         case _         => goto(NotSignedUp(HMRCMTDVAT))
                       }
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCMTDVAT) =>
          def nameToState(name: Option[String]) =
            ConfirmClientPersonalVat(name, Vrn(vatClient.clientIdentifier))

          if (redirectToConfirmVat) goToFinalState(nameToState)
          else
            goToFinalState(nameToState).flatMap { finalState =>
              clientConfirmed(hasActiveRelationship)(agent)(Confirmation(true)).apply(finalState)
            }

        case IdentifyClientBusiness =>
          def nameToState(name: Option[String]) =
            ConfirmClientBusiness(name, Vrn(vatClient.clientIdentifier))
          if (redirectToConfirmVat) goToFinalState(nameToState)
          else
            goToFinalState(nameToState).flatMap { finalState =>
              clientConfirmed(hasActiveRelationship)(agent)(Confirmation(true)).apply(finalState)
            }
      }
    }

    def clientConfirmed(hasActiveRelationship: HasActiveRelationship)(agent: AuthorisedAgent)(
      confirmation: Confirmation): AgentLedDeauthJourneyModel.Transition = {

      def gotoFinalState(clientId: String, service: String, name: Option[String]) =
        if (confirmation.choice) {
          for {
            relationshipIsActive <- hasActiveRelationship(agent.arn, clientId, service)
            result <- if (relationshipIsActive) goto(ConfirmCancel(service, name, clientId))
                     else goto(NotAuthorised(service))
          } yield result
        } else goto(root)

      Transition {
        case ConfirmClientItsa(name, nino)       => gotoFinalState(nino.value, HMRCMTDIT, name)
        case ConfirmClientIrv(name, nino)        => gotoFinalState(nino.value, HMRCPIR, name)
        case ConfirmClientPersonalVat(name, vrn) => gotoFinalState(vrn.value, HMRCMTDVAT, name)
        case ConfirmClientBusiness(name, vrn)    => gotoFinalState(vrn.value, HMRCMTDVAT, name)
      }
    }

    def cancelConfirmed(deleteRelationship: DeleteRelationship, getAgencyName: GetAgencyName)(agent: AuthorisedAgent)(
      confirmation: Confirmation) = Transition {
      case ConfirmCancel(service, clientName, clientId) =>
        if (confirmation.choice) {
          deleteRelationship(service, agent.arn, clientId).flatMap {
            case Some(true) =>
              getAgencyName(agent.arn).flatMap(name => goto(AuthorisationCancelled(service, clientName, name)))
            case _ => goto(ResponseFailed)
          }
        } else goto(root)
    }
  }
}
