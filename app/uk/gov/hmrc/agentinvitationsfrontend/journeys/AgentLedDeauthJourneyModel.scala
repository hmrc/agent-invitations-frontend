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

import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{HMRCMTDIT, HMRCMTDVAT, HMRCPIR}
import uk.gov.hmrc.agentinvitationsfrontend.models.{AuthorisedAgent, ClientType, Confirmation, DOB, ItsaClient, Postcode, Services, VatRegDate}
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
  }

  object Transitions {
    import State._
    import ClientType._


    type CheckPostcodeMatches = (Nino,String) => Future[Option[Boolean]]
    type HasPendingInvitations = (Arn, String, String) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, String) => Future[Boolean]
    type GetClientName = ( String, String) => Future[Option[String]]

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

    def submitIdentifyClientItsa(checkPostcodeMatches: CheckPostcodeMatches,
                             hasPendingInvitations: HasPendingInvitations,
                             hasActiveRelationship: HasActiveRelationship,
                             getClientName: GetClientName)(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      case IdentifyClientPersonal(HMRCMTDIT) => {
        for {
          hasPostCode <- checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode.getOrElse(""))
          r = hasPostCode match {
            case Some(true) => for {
            isPending <- hasPendingInvitations(agent.arn,itsaClient.clientIdentifier, HMRCMTDIT)
            result = isPending match {
              case true => goto(???)
              case false => for {
              hasActiveRelationship <- hasActiveRelationship(agent.arn, HMRCMTDIT, itsaClient.clientIdentifier)
              re = hasActiveRelationship match {
                case true => getClientName(itsaClient.clientIdentifier,HMRCMTDIT)
                  .flatMap(name => goto(ConfirmClientItsa(name,Nino(itsaClient.clientIdentifier),Postcode(itsaClient.postcode.getOrElse("")))))
                case false => ???
              }
              } yield ???
            }
            }yield ???

            case Some(false) => goto(???)
            case None => goto(???)

          }
        } yield ???
      }
    }
  }

}
