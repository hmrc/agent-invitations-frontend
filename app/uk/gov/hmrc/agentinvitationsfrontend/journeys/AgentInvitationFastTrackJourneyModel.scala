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
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

object AgentInvitationFastTrackJourneyModel extends JourneyModel with Logging {

  sealed trait State

  val root: State = Prologue(None, None)

  case class Prologue(failureUrl: Option[String], refererUrl: Option[String]) extends State

  case class CheckDetails(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State {
    def service: Service = fastTrackRequest.service
    def clientType: Option[ClientType] = fastTrackRequest.clientType
  }

  case class MissingDetail(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class SelectClientType(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String], isChanging: Boolean = false) extends State

  case class IdentifyClient(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class ConfirmClientTrust(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String], trustName: String) extends State

  case class InvitationSent(
    clientType: ClientType,
    invitationLink: String,
    continueUrl: Option[String],
    agencyEmail: String,
    service: Service,
    isAltItsa: Option[Boolean] = None
  ) extends State

  case class PendingInvitationExists(fastTrackRequest: AgentFastTrackRequest, agentLink: String, clientName: String, continueUrl: Option[String])
      extends State

  trait AuthExists extends State
  case class ActiveAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends AuthExists
  case class PartialAuthorisationExists(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends AuthExists

  case class ClientNotFound(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class ClientNotSignedUp(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case class ClientNotRegistered(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]) extends State

  case object TryAgainWithoutFastTrack extends State

  case class ConfirmPostcodeCgt(
    fastTrackRequest: AgentFastTrackRequest,
    continueUrl: Option[String],
    postcodeFromDes: Option[String],
    clientName: String
  ) extends State

  case class ConfirmCountryCodeCgt(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String], countryCode: String, clientName: String)
      extends State

  case class ConfirmClientCgt(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String], clientName: String) extends State

  case class ConfirmRegDatePpt(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String], registrationDate: LocalDate, clientName: String)
      extends State

  case class ConfirmClientPpt(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String], clientName: String) extends State

  case class SuspendedAgent(service: Service, continueUrl: Option[String]) extends State

  case object AlreadyCopiedAcrossItsa extends State

  case object ClientInsolventFastTrack extends State
  case object CannotCreateFastTrackRequest extends State

  case class LegacyAuthorisationDetected(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String])
      extends State

  object TransitionEffects {
    type HasPendingInvitations = (Arn, String, Service) => Future[Boolean]
    type HasActiveRelationship = (Arn, String, Service) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, Service) => Future[Option[String]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type GetAgencyEmail = () => Future[String]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type GetSuspensionDetails = () => Future[SuspensionDetails]
    type LegacySaRelationshipStatusFor = (Arn, String) => Future[LegacySaRelationshipResult]
    type GetCbcSubscription = CbcId => Future[Option[SimpleCbcSubscription]]
    type CreateInvitation =
      (Arn, Invitation) => Future[InvitationId]
    type IsAltItsa = (Arn, String) => Future[Boolean]
    type CheckKnownFact = ClientIdSet => Future[KnownFactResult]
  }

  import TransitionEffects._

  case class Transitions(
    getSuspensionDetails: GetSuspensionDetails,
    hasPendingInvitationsFor: HasPendingInvitations,
    hasActiveRelationshipFor: HasActiveRelationship,
    hasPartialAuthorisationFor: HasPartialAuthorisation,
    legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor,
    getClientName: GetClientName,
    getAgentLink: GetAgentLink,
    getAgencyEmail: GetAgencyEmail,
    getCgtSubscription: GetCgtSubscription,
    getPptSubscription: GetPptSubscription,
    getCbcSubscription: GetCbcSubscription,
    createInvitation: CreateInvitation,
    isAltItsa: IsAltItsa,
    checkKnownFact: CheckKnownFact
  )(implicit ec: ExecutionContext) {

    def prologue(failureUrl: Option[String], refererUrl: Option[String]) = Transition { case _ =>
      goto(Prologue(failureUrl, refererUrl))
    }

    def start(continueUrl: Option[String])(agent: AuthorisedAgent)(fastTrackRequest: AgentFastTrackRequest) =
      Transition { case _ =>
        getSuspensionDetails().flatMap { suspensionDetails =>
          if (suspensionDetails.isRegimeSuspended(fastTrackRequest.service)) {
            goto(SuspendedAgent(fastTrackRequest.service, continueUrl))
          } else {
            getStateForNonSuspendedAgent(fastTrackRequest, continueUrl)
          }
        }
      }

    def getStateForNonSuspendedAgent(fastTrackRequest: AgentFastTrackRequest, continueUrl: Option[String]): Future[State] =
      fastTrackRequest match {
        case AgentFastTrackRequest(_, Service.MtdIt, _, _) =>
          val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(ClientType.Personal))
          goto(CheckDetails(updatedPersonalRequest, continueUrl))

        case AgentFastTrackRequest(_, Service.PersonalIncomeRecord, _, _) =>
          val updatedPersonalRequest = fastTrackRequest.copy(clientType = Some(ClientType.Personal))
          goto(CheckDetails(updatedPersonalRequest, continueUrl))

        case AgentFastTrackRequest(_, Service.Trust | Service.TrustNT, _, _) =>
          goto(CheckDetails(fastTrackRequest.copy(clientType = Some(ClientType.Trust)), continueUrl))

        case AgentFastTrackRequest(_, _, _, _) =>
          goto(CheckDetails(fastTrackRequest, continueUrl))
      }

    private def invitationSentState(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String]) =
      for {
        agencyEmail    <- getAgencyEmail()
        _              <- createInvitation(arn, invitation)
        invitationLink <- getAgentLink(arn, fastTrackRequest.clientType)
        result <- fastTrackRequest.clientType match {
                    case Some(ClientType.Personal) =>
                      isAltItsa(arn, fastTrackRequest.clientId.value).map { altItsa =>
                        InvitationSent(ClientType.Personal, invitationLink, continueUrl, agencyEmail, fastTrackRequest.service, Some(altItsa))
                      }
                    case Some(ClientType.Business) =>
                      goto(InvitationSent(ClientType.Business, invitationLink, continueUrl, agencyEmail, fastTrackRequest.service))
                    case Some(ClientType.Trust) =>
                      goto(InvitationSent(ClientType.Business, invitationLink, continueUrl, agencyEmail, fastTrackRequest.service))
                    case None =>
                      throw new RuntimeException(s"No client type found for fast track request: $fastTrackRequest")
                  }
      } yield result

    def confirmedLegacyAuthorisation: Transition =
      Transition { case LegacyAuthorisationDetected(fastTrackRequest, arn, invitation, continueUrl) =>
        invitationSentState(fastTrackRequest, arn, invitation, continueUrl)
      }

    def checkIfPendingOrActiveAndGoto(fastTrackRequest: AgentFastTrackRequest, arn: Arn, invitation: Invitation, continueUrl: Option[String])(
      appConfig: AppConfig
    ): Future[State] = {

      def invitationSent =
        invitationSentState(fastTrackRequest, arn, invitation, continueUrl)
      for {
        hasPendingInvitations <- hasPendingInvitationsFor(arn, fastTrackRequest.clientId.value, fastTrackRequest.service)
        result <- if (hasPendingInvitations) {
                    for {
                      agentLink  <- getAgentLink(arn, fastTrackRequest.clientType)
                      clientName <- getClientName(fastTrackRequest.clientId.value, fastTrackRequest.service)
                      newState <-
                        goto(PendingInvitationExists(fastTrackRequest, agentLink, clientName.getOrElse(fastTrackRequest.clientId.value), continueUrl))
                    } yield newState
                  } else {
                    hasActiveRelationshipFor(arn, fastTrackRequest.clientId.value, fastTrackRequest.service)
                      .flatMap {
                        case true => goto(ActiveAuthorisationExists(fastTrackRequest, continueUrl))
                        case false if appConfig.featuresAltItsa & fastTrackRequest.service == Service.MtdIt =>
                          hasPartialAuthorisationFor(arn, fastTrackRequest.clientId.value).flatMap {
                            case true => goto(PartialAuthorisationExists(fastTrackRequest, continueUrl))
                            case false =>
                              legacySaRelationshipStatusFor(arn, fastTrackRequest.clientId.value).flatMap {
                                case LegacySaRelationshipFoundAndMapped => goto(AlreadyCopiedAcrossItsa)
                                case LegacySaRelationshipFoundNotMapped =>
                                  goto(LegacyAuthorisationDetected(fastTrackRequest, arn, invitation, continueUrl))
                                case LegacySaRelationshipNotFound => invitationSent
                              }
                          }
                        case false => invitationSent
                      }
                  }
      } yield result
    }

    def checkedDetailsNoClientType(agent: AuthorisedAgent) = Transition { case CheckDetails(fastTrackRequest, continueUrl) =>
      goto(SelectClientType(fastTrackRequest, continueUrl))
    }

    def checkedDetailsNoKnownFact(agent: AuthorisedAgent) =
      Transition {
        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.CapitalGains =>
          val cgtRef = CgtRef(fastTrackRequest.clientId.value)

          getCgtSubscription(cgtRef).map {
            case Some(subscription) =>
              if (subscription.isUKBasedClient) {
                ConfirmPostcodeCgt(fastTrackRequest, continueUrl, subscription.postCode, subscription.name)
              } else {
                ConfirmCountryCodeCgt(fastTrackRequest, continueUrl, subscription.countryCode, subscription.name)
              }
            case None =>
              ClientNotFound(fastTrackRequest, continueUrl)
          }

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.Ppt =>
          val pptRef = PptRef(fastTrackRequest.clientId.value)

          getPptSubscription(pptRef).map {
            case Some(subscription) =>
              ConfirmRegDatePpt(fastTrackRequest, continueUrl, subscription.dateOfApplication, subscription.customerName)
            case None =>
              ClientNotFound(fastTrackRequest, continueUrl)
          }

        case CheckDetails(fastTrackRequest, continueUrl) =>
          goto(MissingDetail(fastTrackRequest, continueUrl))

      }

    def confirmPostcodeCgt(agent: AuthorisedAgent)(postcode: Postcode): Transition =
      Transition { case ConfirmPostcodeCgt(fastTrackRequest, continueUrl, postcodeFromDes, name) =>
        val desPostcode = postcodeFromDes.map(withSpacesRemoved)
        val ftrPostcode = withSpacesRemoved(postcode.value)
        if (desPostcode.getOrElse("no_postcode") == ftrPostcode) {
          goto(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some(postcode.value)), continueUrl, name))
        } else {
          goto(ClientNotFound(fastTrackRequest, continueUrl))
        }
      }

    def confirmCountryCodeCgt(agent: AuthorisedAgent)(countryCode: CountryCode): Transition =
      Transition { case ConfirmCountryCodeCgt(fastTrackRequest, continueUrl, countryCodeFromDes, name) =>
        if (countryCodeFromDes.contains(countryCode.value)) {
          goto(ConfirmClientCgt(fastTrackRequest.copy(knownFact = Some(countryCode.value)), continueUrl, name))
        } else {
          goto(ClientNotFound(fastTrackRequest, continueUrl))
        }
      }

    private def withSpacesRemoved(postcode: String): String =
      postcode.replace(" ", "")

    def identifyCgtClient(agent: AuthorisedAgent)(cgtRef: CgtRef): Transition =
      Transition {
        case IdentifyClient(fastTrackRequest, continueUrl) if fastTrackRequest.service == Service.CapitalGains =>
          getCgtSubscription(CgtRef(cgtRef.value)) map {
            case Some(subscription) =>
              val ftrKnownFact = fastTrackRequest.knownFact.map(withSpacesRemoved)
              val newFtr = fastTrackRequest.copy(clientId = cgtRef)
              if (subscription.isUKBasedClient) {
                val desPostcode = subscription.postCode.map(withSpacesRemoved)
                if (ftrKnownFact.contains(desPostcode.getOrElse("no_postcode"))) {
                  ConfirmClientCgt(newFtr, continueUrl, subscription.name)
                } else ConfirmPostcodeCgt(newFtr, continueUrl, subscription.postCode, subscription.name)
              } else {
                val desCountryCode = withSpacesRemoved(subscription.countryCode)
                if (ftrKnownFact.contains(desCountryCode)) {
                  ConfirmClientCgt(newFtr, continueUrl, subscription.name)
                } else ConfirmCountryCodeCgt(newFtr, continueUrl, subscription.countryCode, subscription.name)
              }
            case None => ClientNotFound(fastTrackRequest, continueUrl)
          }
      }

    def confirmRegDatePpt(agent: AuthorisedAgent)(regDate: String): Transition =
      Transition { case ConfirmRegDatePpt(fastTrackRequest, continueUrl, actualRegDate, name) =>
        if (actualRegDate.equals(LocalDate.parse(regDate))) {
          goto(ConfirmClientPpt(fastTrackRequest, continueUrl, name))
        } else {
          goto(ClientNotFound(fastTrackRequest, continueUrl))
        }
      }

    def identifyPptClient(getPptCustomerName: PptRef => Future[Option[String]])(agent: AuthorisedAgent)(pptClient: PptClient): Transition =
      Transition {
        case IdentifyClient(fastTrackRequest, continueUrl) if fastTrackRequest.service == Service.Ppt =>
          for {
            knownFactResult <- checkKnownFact(pptClient)
            mCustomerName <- if (knownFactResult.isOk) getPptCustomerName(PptRef(fastTrackRequest.clientId.value))
                             else Future.successful(None)
          } yield mCustomerName match {
            case Some(customerName) => ConfirmClientPpt(fastTrackRequest, continueUrl, customerName)
            case None               => ClientNotFound(fastTrackRequest, continueUrl)
          }
      }

    def identifyCbcClient(appConfig: AppConfig)(agent: AuthorisedAgent)(cbcClient: CbcClient): Transition =
      Transition {
        case IdentifyClient(ftr, continueUrl) if Set[Service](Service.Cbc, Service.CbcNonUk).contains(ftr.service) =>
          for {
            knownFactResult <- checkKnownFact(cbcClient)
            nextState <- if (knownFactResult.isOk) {
                           for {
                             maybeSubscription <- getCbcSubscription(cbcClient.cbcId)
                             subscription =
                               maybeSubscription.getOrElse(throw new RuntimeException(s"CBC subscription for ${cbcClient.cbcId} not found!"))
                             adjustedService = if (subscription.isGBUser) Service.Cbc else Service.CbcNonUk
                             result <- checkIfPendingOrActiveAndGoto(
                                         ftr,
                                         agent.arn,
                                         Invitation(ftr.clientType, adjustedService, cbcClient.cbcId),
                                         continueUrl
                                       )(appConfig)
                           } yield result
                         } else {
                           Future.successful(ClientNotFound(ftr, continueUrl))
                         }
          } yield nextState
      }

    def identifyPillar2Client(appConfig: AppConfig)(agent: AuthorisedAgent)(pillar2Client: Pillar2Client): Transition =
      Transition {
        case IdentifyClient(ftr, continueUrl) if ftr.service == Service.Pillar2 =>
          for {
            knownFactResult <- checkKnownFact(pillar2Client)
            nextState <- if (knownFactResult.isOk) {
                           checkIfPendingOrActiveAndGoto(
                             ftr,
                             agent.arn,
                             Invitation(ftr.clientType, Service.Pillar2, pillar2Client.plrId),
                             continueUrl
                           )(appConfig)
                         } else {
                           Future.successful(ClientNotFound(ftr, continueUrl))
                         }
          } yield nextState
      }

    def checkedDetailsChangeInformation(agent: AuthorisedAgent): AgentInvitationFastTrackJourneyModel.Transition =
      Transition {
        case CheckDetails(ftr, continueUrl) if ftr.clientType.isEmpty =>
          goto(SelectClientType(ftr, continueUrl, isChanging = true))

        case CheckDetails(ftr, continueUrl) =>
          goto(IdentifyClient(ftr, continueUrl))
      }

    def checkedDetailsAllInformation(appConfig: AppConfig)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {
        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.MtdIt =>
          if (confirmation.choice) {
            checkKnownFact(ItsaClient(Nino(fastTrackRequest.clientId.value), fastTrackRequest.knownFact.getOrElse("")))
              .flatMap {
                case Pass =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(fastTrackRequest.clientType, Service.MtdIt, fastTrackRequest.clientId),
                    continueUrl
                  )(appConfig)
                case Fail(NotFound) =>
                  if (appConfig.featuresAltItsa) goto(ClientNotRegistered(fastTrackRequest, continueUrl))
                  else goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
                case Fail(NotMatched) | _ => goto(ClientNotFound(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.PersonalIncomeRecord =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkKnownFact(IrvClient(Nino(fastTrackRequest.clientId.value), knownFact))
              .flatMap {
                case Pass =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(fastTrackRequest.clientType, Service.PersonalIncomeRecord, fastTrackRequest.clientId),
                    continueUrl
                  )(appConfig)
                case _ => goto(ClientNotFound(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl)
            if cdc.service == Service.Vat && cdc.clientType.exists(ct => ct == ClientType.Personal || ct == ClientType.Business) =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkKnownFact(VatClient(Vrn(fastTrackRequest.clientId.value), knownFact))
              .flatMap {
                case Pass =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(cdc.clientType, Service.Vat, fastTrackRequest.clientId),
                    continueUrl
                  )(appConfig)
                case Fail(VatClientInsolvent)     => goto(ClientInsolventFastTrack)
                case Fail(VatMigrationInProgress) => goto(CannotCreateFastTrackRequest)
                case Fail(NotFound)               => goto(ClientNotSignedUp(fastTrackRequest, continueUrl))
                case Fail(NotMatched) | _         => goto(ClientNotFound(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if List(Service.Trust, Service.TrustNT).contains(cdc.service) =>
          if (confirmation.choice) {
            getClientName(fastTrackRequest.clientId.value, fastTrackRequest.service).flatMap {
              case Some(_) =>
                val trustInvitation = fastTrackRequest.clientId match {
                  case Utr(utr) if Utr.isValid(utr) => Invitation(Some(ClientType.Trust), Service.Trust, Utr(utr))
                  case Urn(urn) if Urn.isValid(urn) => Invitation(Some(ClientType.Trust), Service.TrustNT, Urn(urn))
                }
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  trustInvitation,
                  continueUrl
                )(appConfig)
              case None => goto(ClientNotFound(fastTrackRequest, continueUrl))
            }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.CapitalGains =>
          if (confirmation.choice) {
            getCgtSubscription(CgtRef(fastTrackRequest.clientId.value)).flatMap {
              case Some(subscription) =>
                val (knownFactEmptyState, field) = if (subscription.isUKBasedClient) {
                  (ConfirmPostcodeCgt(fastTrackRequest, continueUrl, subscription.postCode, subscription.name), subscription.postCode)
                } else
                  (ConfirmCountryCodeCgt(fastTrackRequest, continueUrl, subscription.countryCode, subscription.name), Some(subscription.countryCode))
                fastTrackRequest.knownFact
                  .fold(goto(knownFactEmptyState): Future[State]) { knownFact =>
                    val ftrKnownFact = withSpacesRemoved(knownFact)
                    val desKNownFact = field.map(withSpacesRemoved)
                    if (ftrKnownFact == desKNownFact.getOrElse("no_postcode"))
                      checkIfPendingOrActiveAndGoto(
                        fastTrackRequest,
                        agent.arn,
                        Invitation(fastTrackRequest.clientType, Service.CapitalGains, fastTrackRequest.clientId),
                        continueUrl
                      )(appConfig)
                    else goto(ClientNotFound(fastTrackRequest, continueUrl))
                  }
              case None => goto(ClientNotFound(fastTrackRequest, continueUrl))
            }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.Ppt =>
          if (confirmation.choice) {
            val knownFact = fastTrackRequest.knownFact.getOrElse("")
            checkKnownFact(PptClient(PptRef(fastTrackRequest.clientId.value), knownFact))
              .flatMap {
                case Pass =>
                  checkIfPendingOrActiveAndGoto(
                    fastTrackRequest,
                    agent.arn,
                    Invitation(fastTrackRequest.clientType, Service.Ppt, fastTrackRequest.clientId),
                    continueUrl
                  )(appConfig)
                case _ => goto(ClientNotFound(fastTrackRequest, continueUrl))
              }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if Seq(Service.Cbc, Service.CbcNonUk).contains(cdc.service) =>
          if (confirmation.choice) {
            val cbcId = CbcId(fastTrackRequest.clientId.value)
            val emailKnownFact = fastTrackRequest.knownFact.getOrElse("")

            checkKnownFact(CbcClient(cbcId, emailKnownFact)).flatMap {
              case Pass =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  Invitation(fastTrackRequest.clientType, cdc.service, fastTrackRequest.clientId),
                  continueUrl
                )(appConfig)
              case _ =>
                goto(ClientNotFound(fastTrackRequest, continueUrl))
            }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))

        case cdc @ CheckDetails(fastTrackRequest, continueUrl) if cdc.service == Service.Pillar2 =>
          if (confirmation.choice) {
            val plrId = PlrId(fastTrackRequest.clientId.value)
            val regDate = fastTrackRequest.knownFact.getOrElse("")
            checkKnownFact(Pillar2Client(plrId, regDate)).flatMap {
              case Pass =>
                checkIfPendingOrActiveAndGoto(
                  fastTrackRequest,
                  agent.arn,
                  Invitation(fastTrackRequest.clientType, cdc.service, fastTrackRequest.clientId),
                  continueUrl
                )(appConfig)
              case _ =>
                goto(ClientNotFound(fastTrackRequest, continueUrl))
            }
          } else goto(IdentifyClient(fastTrackRequest, continueUrl))
      }

    def identifiedClientItsa(appConfig: AppConfig)(agent: AuthorisedAgent)(itsaClient: ItsaClient) =
      Transition {
        case IdentifyClient(ftr, continueUrl) if ftr.clientType.contains(ClientType.Personal) && ftr.service == Service.MtdIt =>
          val newState =
            CheckDetails(ftr.copy(service = Service.MtdIt, clientId = itsaClient.nino, knownFact = Some(itsaClient.postcode)), continueUrl)
          checkedDetailsAllInformation(appConfig)(agent)(Confirmation(true))
            .apply(newState)
      }

    def identifiedClientIrv(appConfig: AppConfig)(agent: AuthorisedAgent)(irvClient: IrvClient) =
      Transition {
        case IdentifyClient(ftr, continueUrl) if ftr.clientType.contains(ClientType.Personal) && ftr.service == Service.PersonalIncomeRecord =>
          val newState =
            CheckDetails(ftr.copy(service = Service.PersonalIncomeRecord, clientId = irvClient.nino, knownFact = Some(irvClient.dob)), continueUrl)
          checkedDetailsAllInformation(appConfig)(agent)(Confirmation(true))
            .apply(newState)
      }

    def identifiedClientVat(appConfig: AppConfig)(agent: AuthorisedAgent)(vatClient: VatClient) =
      Transition {
        case IdentifyClient(ftr, continueUrl) if ftr.clientType.contains(ClientType.Personal) && ftr.service == Service.Vat =>
          val newState = CheckDetails(
            ftr.copy(
              service = Service.Vat,
              clientType = Some(ClientType.Personal),
              clientId = vatClient.vrn,
              knownFact = Some(vatClient.registrationDate)
            ),
            continueUrl
          )
          checkedDetailsAllInformation(appConfig)(agent)(Confirmation(true))
            .apply(newState)

        case IdentifyClient(ftr, continueUrl) if ftr.clientType.contains(ClientType.Business) && ftr.service == Service.Vat =>
          val newState = CheckDetails(
            ftr.copy(clientId = vatClient.vrn, knownFact = Some(vatClient.registrationDate)),
            continueUrl
          )
          checkedDetailsAllInformation(appConfig)(agent)(Confirmation(true))
            .apply(newState)

        case IdentifyClient(ftr, continueUrl) if ftr.clientType.isEmpty =>
          val newState =
            CheckDetails(ftr.copy(clientId = vatClient.vrn, knownFact = Some(vatClient.registrationDate)), continueUrl)
          checkedDetailsNoClientType(agent).apply(newState)
      }

    def showConfirmTrustClient(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyClient(ftr, continueUrl) if List(Service.Trust, Service.TrustNT).contains(ftr.service) =>
          getClientName(trustClient.taxId.value, Service.Trust).flatMap {
            case Some(name) =>
              val trustClientId = ftr.clientId match {
                case Utr(_) => Utr(trustClient.taxId.value)
                case Urn(_) => Urn(trustClient.taxId.value)
              }
              goto(ConfirmClientTrust(ftr.copy(clientId = trustClientId), continueUrl, name))
            case None => goto(ClientNotFound(ftr, continueUrl))
          }
      }

    def submitConfirmTrustClient(appConfig: AppConfig)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition { case ConfirmClientTrust(ftr, continueUrl, trustName) =>
        if (confirmation.choice) {
          val trustInvitation = ftr.clientId match {
            case Utr(utr) if Utr.isValid(utr) => Invitation(Some(ClientType.Trust), Service.Trust, Utr(utr))
            case Urn(urn) if Urn.isValid(urn) => Invitation(Some(ClientType.Trust), Service.TrustNT, Urn(urn))
          }
          checkIfPendingOrActiveAndGoto(
            ftr,
            agent.arn,
            trustInvitation,
            continueUrl
          )(appConfig)
        } else {
          goto(IdentifyClient(ftr, continueUrl))
        }
      }

    def submitConfirmClientCgt(appConfig: AppConfig)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition { case ConfirmClientCgt(ftr, continueUrl, cgtName) =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ftr,
            agent.arn,
            Invitation(ftr.clientType, Service.CapitalGains, ftr.clientId),
            continueUrl
          )(appConfig)
        } else {
          goto(IdentifyClient(ftr, continueUrl))
        }
      }

    def submitConfirmClientPpt(appConfig: AppConfig)(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition { case ConfirmClientPpt(ftr, continueUrl, pptName) =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(
            ftr,
            agent.arn,
            Invitation(ftr.clientType, Service.Ppt, ftr.clientId),
            continueUrl
          )(appConfig)
        } else {
          goto(IdentifyClient(ftr, continueUrl))
        }
      }

    def moreDetailsSupplied(appConfig: AppConfig)(agent: AuthorisedAgent)(suppliedKnownFact: String) =
      Transition { case MissingDetail(ftRequest, continueUrl) =>
        // override client type if VAT (unsure why this is, but this is to maintain legacy behaviour)
        val clientType = if (ftRequest.service == Service.Vat) {
          if (ftRequest.clientType.contains(ClientType.Personal))
            Some(ClientType.Personal)
          else
            Some(ClientType.Business)
        } else ftRequest.clientType
        val newState =
          CheckDetails(ftRequest.copy(clientType = clientType, knownFact = Some(suppliedKnownFact)), continueUrl)

        checkedDetailsAllInformation(appConfig)(agent)(Confirmation(true))
          .apply(newState)
      }

    def tryAgainNotMatchedKnownFact(agent: AuthorisedAgent) =
      Transition { case ClientNotFound(fastTrackRequest, continueUrl) =>
        val ftrWithoutKF = fastTrackRequest.copy(knownFact = None)
        fastTrackRequest match {
          case AgentFastTrackRequest(None, Service.Vat, _, _) =>
            goto(SelectClientType(fastTrackRequest, continueUrl))

          case AgentFastTrackRequest(_, Service.CapitalGains | Service.Ppt, _, _) =>
            goto(IdentifyClient(fastTrackRequest, continueUrl))

          case AgentFastTrackRequest(_, _, _, _) =>
            goto(IdentifyClient(ftrWithoutKF, continueUrl))
        }
      }

    def selectedClientType(appConfig: AppConfig)(agent: AuthorisedAgent)(suppliedClientType: String) =
      Transition {
        case SelectClientType(ftr, continueUrl, isChanging) if ftr.service == Service.Vat =>
          if (ftr.knownFact.isDefined) {
            val newState =
              CheckDetails(ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))), continueUrl)
            checkedDetailsAllInformation(appConfig)(agent)(Confirmation(true))
              .apply(newState)
          } else {
            val newState =
              CheckDetails(ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType))), continueUrl)
            if (isChanging) checkedDetailsChangeInformation(agent).apply(newState)
            else checkedDetailsNoKnownFact(agent).apply(newState)
          }

        case SelectClientType(ftr, continueUrl, isChanging) if ftr.service == Service.CapitalGains =>
          val newFtr = ftr.copy(clientType = Some(if (suppliedClientType == "trust") ClientType.Trust else ClientType.Personal))
          getCgtSubscription(CgtRef(ftr.clientId.value)).map {
            case Some(subscription) =>
              if (isChanging) {
                IdentifyClient(newFtr, continueUrl)
              } else if (subscription.isUKBasedClient) {
                ftr.knownFact.fold(ConfirmPostcodeCgt(newFtr, continueUrl, subscription.postCode, subscription.name): State) { postcode =>
                  val desPostcode = withSpacesRemoved(subscription.postCode.getOrElse(""))
                  if (withSpacesRemoved(postcode) == desPostcode) ConfirmClientCgt(newFtr, continueUrl, subscription.name)
                  else ClientNotFound(newFtr, continueUrl)
                }
              } else {
                ConfirmCountryCodeCgt(newFtr, continueUrl, subscription.countryCode, subscription.name)
              }
            case None =>
              ClientNotFound(newFtr, continueUrl)
          }

        case SelectClientType(ftr, continueUrl, isChanging) if ftr.service == Service.Ppt =>
          val newFtr = ftr.copy(clientType = Some(ClientType.toEnum(suppliedClientType)))
          getPptSubscription(PptRef(ftr.clientId.value)).map {
            case Some(subscription) =>
              if (isChanging)
                IdentifyClient(newFtr, continueUrl)
              else
                newFtr.knownFact
                  .fold(ConfirmRegDatePpt(newFtr, continueUrl, subscription.dateOfApplication, subscription.customerName): State) { regDate =>
                    if (subscription.dateOfApplication == LocalDate.parse(regDate))
                      ConfirmClientPpt(newFtr, continueUrl, subscription.customerName)
                    else ClientNotFound(newFtr, continueUrl)
                  }
            case None =>
              ClientNotFound(newFtr, continueUrl)
          }
      }
  }
}
