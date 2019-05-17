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
    case class ConfirmClientItsa(clientName: Option[String], nino: Nino, postcode: Postcode) extends State
    case class ConfirmClientIrv(clientName: Option[String], nino: Nino, dob: DOB) extends State
    case class ConfirmClientPersonalVat(clientName: Option[String], vrn: Vrn, vatRegDate: VatRegDate) extends State
    case class ConfirmClientBusiness(clientName: Option[String], vrn: Vrn, vatRegDate: VatRegDate) extends State
    case object KnownFactNotMatched extends State
    case class NotSignedUp(serviceId: String) extends State
    case object CannotCreateRequest extends State
  }

  object Transitions {

    import ClientType._
    import State._

    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]] //String = postcode
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = (String, String) => Future[Option[String]] // client id and service
    type DOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]
    type VatRegDateMatches = (Vrn, LocalDate) => Future[Option[Int]]

    def showSelectClientType(agent: AuthorisedAgent) = Transition {
      case _ => goto(SelectClientType)
    }

    def chosenClientType(agent: AuthorisedAgent)(clientType: ClientType) = Transition {
      case SelectClientType if clientType == personal => {
        val enabledPersonalServices =
          if (agent.isWhitelisted)
            Set(HMRCPIR, HMRCMTDIT, HMRCMTDVAT)
          else
            Set(HMRCMTDIT, HMRCMTDVAT)
        goto(SelectServicePersonal(enabledPersonalServices))
      }
      case SelectClientType if clientType == business => goto(SelectServiceBusiness)
    }

    def chosenPersonalService(agent: AuthorisedAgent)(service: String) = Transition {
      case SelectServicePersonal(_) => goto(IdentifyClientPersonal(service))
    }

    def chosenBusinessService(agent: AuthorisedAgent)(confirmation: Confirmation) = Transition {
      case SelectServiceBusiness if confirmation.choice => goto(IdentifyClientBusiness)
      case SelectServiceBusiness                        => goto(SelectClientType)
    }

    def submitIdentifyClientItsa(checkPostcodeMatches: CheckPostcodeMatches, getClientName: GetClientName)(
      agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyClientPersonal(HMRCMTDIT) => {
        def nameToState(name: Option[String]) =
          ConfirmClientItsa(name, Nino(itsaClient.clientIdentifier), Postcode(itsaClient.postcode.getOrElse("")))

        submitIdentifyClient[String](checkPostcodeMatches, getClientName, nameToState)(
          Nino(itsaClient.clientIdentifier),
          itsaClient.postcode.getOrElse(""),
          HMRCMTDIT)
      }
    }

    def submitIdentifyClientIrv(DOBMatches: DOBMatches, getClientName: GetClientName)(agent: AuthorisedAgent)(
      irvClient: IrvClient) = Transition {
      case IdentifyClientPersonal(HMRCPIR) =>
        def nameToState(name: Option[String]) =
          ConfirmClientIrv(name, Nino(irvClient.clientIdentifier), DOB(irvClient.dob.getOrElse("")))

        submitIdentifyClient[LocalDate](DOBMatches, getClientName, nameToState)(
          Nino(irvClient.clientIdentifier),
          LocalDate.parse(irvClient.dob.getOrElse("")),
          HMRCPIR)
    }

    def submitIdentifyClient[T](
      kfMatches: (Nino, T) => Future[Option[Boolean]],
      getClientName: GetClientName,
      successState: Option[String] => State)(clientId: Nino, kf: T, service: String): Future[State] =
      for {
        hasKnownFact <- kfMatches(clientId, kf)
        finalState <- hasKnownFact match {
                       case Some(true) =>
                         getClientName(clientId.value, service).flatMap(name => goto(successState(name)))
                       case Some(false) => goto(KnownFactNotMatched)
                       case None        => goto(NotSignedUp(service))
                     }
      } yield finalState

    def submitIdentityClientVat(vatRegDateMatches: VatRegDateMatches, getClientName: GetClientName)(
      agent: AuthorisedAgent)(vatClient: VatClient): AgentLedDeauthJourneyModel.Transition = {
      def goToFinalState(finalState: Option[String] => State): Future[State] =
        for {
          hasVatRegDate <- vatRegDateMatches(
                            Vrn(vatClient.clientIdentifier),
                            LocalDate.parse(vatClient.registrationDate.getOrElse("")))
          finalState <- hasVatRegDate match {
                         case Some(204) =>
                           getClientName(vatClient.clientIdentifier, HMRCMTDVAT).flatMap(name => goto(finalState(name)))
                         case Some(403) => goto(KnownFactNotMatched)
                         case Some(423) => goto(CannotCreateRequest)
                         case None      => goto(NotSignedUp(HMRCMTDVAT))
                       }
        } yield finalState

      Transition {
        case IdentifyClientPersonal(HMRCMTDVAT) =>
          def nameToState(name: Option[String]) =
            ConfirmClientPersonalVat(
              name,
              Vrn(vatClient.clientIdentifier),
              VatRegDate(vatClient.registrationDate.getOrElse("")))
          goToFinalState(nameToState)

        case IdentifyClientBusiness =>
          def nameToState(name: Option[String]) =
            ConfirmClientBusiness(
              name,
              Vrn(vatClient.clientIdentifier),
              VatRegDate(vatClient.registrationDate.getOrElse("")))
          goToFinalState(nameToState)
      }
    }
  }
}
