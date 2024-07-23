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
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.models.KnownFactResult._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.{ExecutionContext, Future}

object AgentInvitationJourneyModel extends JourneyModel with Logging {

  sealed trait State

  val root: State = SelectClientType(Set.empty)

  case class SelectClientType(basket: Basket) extends State

  case class SelectService(clientType: ClientType, services: Set[Service], basket: Basket) extends State

  case class IdentifyClient(clientType: ClientType, service: Service, basket: Basket) extends State

  case class PendingInvitationExists(clientType: ClientType, clientName: String, agentLink: String, basket: Basket) extends State

  trait AuthorisationExists extends State
  case class ActiveAuthorisationExists(clientType: ClientType, service: Service, basket: Basket) extends AuthorisationExists
  case class PartialAuthorisationExists(basket: Basket) extends AuthorisationExists
  case class LegacyAuthorisationDetected(basket: Basket) extends State

  trait NotFound extends State
  case class KnownFactNotMatched(basket: Basket) extends NotFound
  case class TrustNotFound(basket: Basket) extends NotFound
  case class CgtRefNotFound(cgtRef: CgtRef, basket: Basket) extends NotFound
  case class PptRefNotFound(pptRef: PptRef, basket: Basket) extends NotFound

  case class CannotCreateRequest(basket: Basket) extends State

  case class ConfirmClient(request: AuthorisationRequest, basket: Basket, clientInsolvent: Option[Boolean] = None) extends State {
    def service = request.invitation.service
    def clientType = request.invitation.clientType
  }
  case class ConfirmPostcodeCgt(cgtRef: CgtRef, clientType: ClientType, basket: Basket, postcode: Option[String], clientName: String) extends State
  case class ConfirmCountryCodeCgt(cgtRef: CgtRef, clientType: ClientType, basket: Basket, countryCode: String, clientName: String) extends State

  trait Review extends State
  case class ReviewAuthorisations(clientType: ClientType, services: Set[Service], basket: Basket) extends Review

  case class SomeAuthorisationsFailed(invitationLink: String, continueUrl: Option[String], agencyEmail: String, basket: Basket) extends State
  case class AllAuthorisationsFailed(basket: Basket) extends State
  case class DeleteAuthorisationRequest(clientType: ClientType, authorisationRequest: AuthorisationRequest, basket: Basket) extends State

  case class InvitationSent(
    clientType: ClientType,
    invitationLink: String,
    continueUrl: Option[String],
    agencyEmail: String,
    services: Set[Service],
    isAltItsa: Option[Boolean] = None
  ) extends State

  case class ClientNotSignedUp(service: Service, basket: Basket) extends State
  case object AllAuthorisationsRemoved extends State
  case class AgentSuspended(suspendedService: Service, basket: Basket) extends State
  case class ClientNotRegistered(basket: Basket) extends State
  case object AlreadyCopiedAcrossItsa extends State
  case class ClientInsolvent(basket: Basket) extends State

  type Basket = Set[AuthorisationRequest]

  object TransitionEffects {
    type HasPendingInvitations = (Arn, String, Service) => Future[Boolean]
    type CreateInvitationSent = (String, String, Arn, Basket) => Future[State]
    type HasActiveRelationship = (Arn, String, Service) => Future[Boolean]
    type HasPartialAuthorisation = (Arn, String) => Future[Boolean]
    type GetClientName = (String, Service) => Future[Option[String]]
    type CreateMultipleInvitations =
      (Arn, Set[AuthorisationRequest]) => Future[Set[AuthorisationRequest]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type GetAgencyEmail = () => Future[String]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetSuspensionDetails = () => Future[SuspensionDetails]
    type LegacySaRelationshipStatusFor = (Arn, String) => Future[LegacySaRelationshipResult]
    type GetCbcSubscription = CbcId => Future[Option[SimpleCbcSubscription]]
    type CheckKnownFact = ClientIdSet => Future[KnownFactResult]
  }

  import TransitionEffects._

  case class Transitions(
    appConfig: AppConfig,
    featureFlags: FeatureFlags,
    getSuspensionDetails: GetSuspensionDetails,
    hasPendingInvitationsFor: HasPendingInvitations,
    hasActiveRelationshipFor: HasActiveRelationship,
    hasPartialAuthorisationFor: HasPartialAuthorisation,
    legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor,
    hasAltItsaInvitations: HasPartialAuthorisation,
    getClientName: GetClientName,
    getAgentLink: GetAgentLink,
    getAgencyEmail: GetAgencyEmail,
    createMultipleInvitations: CreateMultipleInvitations,
    createInvitationSent: CreateInvitationSent,
    getCgtSubscription: GetCgtSubscription,
    getCbcSubscription: GetCbcSubscription,
    checkKnownFact: CheckKnownFact
  )(implicit ec: ExecutionContext) {
    val start: AgentInvitationJourneyModel.Transition = AgentInvitationJourneyModel.start

    def selectedClientType(agent: AuthorisedAgent)(clientTypeStr: String): Transition = Transition { case SelectClientType(basket) =>
      val clientType = ClientType.toEnum(clientTypeStr)
      val services = Services.supportedServicesFor(clientType).filter(featureFlags.isServiceEnabled)
      goto(SelectService(clientType, services, basket))
    }

    def gotoIdentify(serviceEnabled: Boolean, service: Service, identifyClientState: State, suspendedState: State): Future[State] =
      if (serviceEnabled) {
        getSuspensionDetails().flatMap { suspensionDetails =>
          if (suspensionDetails.isRegimeSuspended(service)) {
            goto(suspendedState)
          } else {
            goto(identifyClientState)
          }
        }
      } else {
        fail(new Exception(s"Service: ${service.id} feature flag is switched off"))
      }

    def selectedService(agent: AuthorisedAgent)(mService: Option[Service]) =
      Transition { case SelectService(clientType, services, basket) =>
        mService match {
          case None => // user selected "no" to final service
            if (basket.isEmpty)
              goto(SelectClientType(basket)) // if no services in basket, and user also declined the final service, go back to SelectClientType
            else goto(ReviewAuthorisations(clientType, services, basket)) // otherwise proceed to review
          case Some(service) if services.contains(service) =>
            gotoIdentify(
              featureFlags.isServiceEnabled(service),
              service,
              IdentifyClient(clientType, service, basket),
              AgentSuspended(service, basket)
            )
          case _ => goto(SelectService(clientType, services, basket))
        }
      }

    def selectedServiceMulti(agent: AuthorisedAgent)(service: Service) =
      selectedService(agent)(Some(service))

    def identifiedTrustClient(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyClient(ClientType.Trust, trustService, basket) if List(Service.Trust, Service.TrustNT).contains(trustService) =>
          getClientName(trustClient.taxId.value, Service.Trust).flatMap {
            case Some(name) =>
              trustClient.taxId match {
                case Utr(_) =>
                  goto(
                    ConfirmClient(AuthorisationRequest(name, Invitation(Some(ClientType.Trust), Service.Trust, Utr(trustClient.taxId.value))), basket)
                  )
                case Urn(_) =>
                  goto(
                    ConfirmClient(
                      AuthorisationRequest(name, Invitation(Some(ClientType.Trust), Service.TrustNT, Urn(trustClient.taxId.value))),
                      basket
                    )
                  )

              }
            case None => goto(TrustNotFound(basket))
          }
      }

    def identifyCgtClient(agent: AuthorisedAgent)(cgtRef: CgtRef): AgentInvitationJourneyModel.Transition =
      Transition { case IdentifyClient(clientType, Service.CapitalGains, basket) =>
        require(Services.isSupported(clientType, Service.CapitalGains))
        getCgtSubscription(cgtRef).map {
          case Some(subscription) =>
            if (subscription.isUKBasedClient) {
              ConfirmPostcodeCgt(cgtRef, clientType, basket, subscription.postCode, subscription.name)
            } else {
              ConfirmCountryCodeCgt(cgtRef, clientType, basket, subscription.countryCode, subscription.name)
            }
          case None =>
            CgtRefNotFound(cgtRef, basket)
        }
      }

    def identifyPptClient(agent: AuthorisedAgent)(pptClient: PptClient): AgentInvitationJourneyModel.Transition = {
      def handle(mkState: String => State, basket: Basket) =
        for {
          knownFactResult <- checkKnownFact(pptClient)
          mCustomerName <- knownFactResult match {
                             case Pass => getClientName(pptClient.pptRef.value, Service.Ppt)
                             case _    => Future.successful(None)
                           }
        } yield mCustomerName match {
          case Some(customerName) => mkState(customerName)
          case None               => PptRefNotFound(pptClient.pptRef, basket)
        }

      Transition { case IdentifyClient(clientType, Service.Ppt, basket) =>
        handle(
          customerName => ConfirmClient(AuthorisationRequest(customerName, Invitation(Some(clientType), Service.Ppt, pptClient.pptRef)), basket),
          basket
        )
      }
    }

    def identifyCbcClient(agent: AuthorisedAgent)(cbcClient: CbcClient): AgentInvitationJourneyModel.Transition =
      Transition { case IdentifyClient(clientType, Service.Cbc | Service.CbcNonUk, basket) =>
        for {
          knownFactResult <- checkKnownFact(cbcClient)
          nextState <- knownFactResult match {
                         case Pass =>
                           for {
                             maybeSubscription <- getCbcSubscription(cbcClient.cbcId)
                             subscription =
                               maybeSubscription.getOrElse(throw new RuntimeException(s"CBC subscription for ${cbcClient.cbcId} not found!"))
                             adjustedService = if (subscription.isGBUser) Service.Cbc else Service.CbcNonUk
                             clientName = subscription.anyAvailableName.getOrElse(cbcClient.cbcId.value)
                           } yield ConfirmClient(
                             AuthorisationRequest(clientName, Invitation(Some(clientType), adjustedService, cbcClient.cbcId)),
                             basket
                           )
                         case _ => Future.successful(KnownFactNotMatched(basket))
                       }
        } yield nextState
      }

    def identifyPillar2Client(agent: AuthorisedAgent)(pillar2Client: Pillar2Client): AgentInvitationJourneyModel.Transition =
      Transition { case IdentifyClient(clientType, Service.Pillar2, basket) =>
        for {
          knownFactResult <- checkKnownFact(pillar2Client)
          nextState <- if (knownFactResult.isOk) {
                         for {
                           mClientName <- getClientName(pillar2Client.plrId.value, Service.Pillar2)
                           clientName = mClientName.getOrElse(pillar2Client.plrId.value)
                         } yield ConfirmClient(
                           AuthorisationRequest(clientName, Invitation(Some(clientType), Service.Pillar2, pillar2Client.plrId)),
                           basket
                         )
                       } else {
                         Future.successful(KnownFactNotMatched(basket))
                       }
        } yield nextState
      }

    private def removeSpaceFromPostcode(postcode: String): String =
      postcode.replace(" ", "")

    def confirmPostcodeCgt(agent: AuthorisedAgent)(postcode: Postcode): Transition =
      Transition { case ConfirmPostcodeCgt(cgtRef, clientType, basket, postcodeFromDes, name) =>
        val userPostcodeWithoutSpace = removeSpaceFromPostcode(postcode.value)
        val desPostcodeWithoutSpace = removeSpaceFromPostcode(postcodeFromDes.getOrElse("no_des_postcode"))

        if (desPostcodeWithoutSpace == userPostcodeWithoutSpace) {
          goto(ConfirmClient(AuthorisationRequest(name, Invitation(Some(clientType), Service.CapitalGains, cgtRef)), basket))
        } else {
          logger.warn(s"CGT postcode match failed. DES postcode did not match user entered postcode")
          goto(KnownFactNotMatched(basket))
        }
      }

    def confirmCountryCodeCgt(agent: AuthorisedAgent)(countryCode: CountryCode): Transition =
      Transition { case ConfirmCountryCodeCgt(cgtRef, clientType, basket, countryCodeFromDes, name) =>
        if (countryCodeFromDes.contains(countryCode.value)) {
          goto(ConfirmClient(AuthorisationRequest(name, Invitation(Some(clientType), Service.CapitalGains, cgtRef)), basket))
        } else {
          goto(KnownFactNotMatched(basket))
        }
      }

    // format: off
    def identifiedItsaClient(agent: AuthorisedAgent)(itsaClient: ItsaClient) = Transition {
      // format: on
      case IdentifyClient(ClientType.Personal, Service.MtdIt, basket) =>
        for {
          knownFactResult <- checkKnownFact(itsaClient)
          endState <- knownFactResult match {
                        case Pass =>
                          getClientName(itsaClient.nino.value, Service.MtdIt).flatMap { clientName =>
                            goto(
                              ConfirmClient(
                                AuthorisationRequest(clientName.getOrElse(""), Invitation(Some(ClientType.Personal), Service.MtdIt, itsaClient.nino)),
                                basket
                              )
                            )
                          }
                        case Fail(NotFound) =>
                          if (appConfig.featuresAltItsa) goto(ClientNotRegistered(basket)) else goto(ClientNotSignedUp(Service.MtdIt, basket))
                        case Fail(NotMatched) | _ => goto(KnownFactNotMatched(basket))
                      }
        } yield endState
    }

    // format: off
    def identifiedVatClient(agent: AuthorisedAgent)
                           (vatClient: VatClient) = Transition {
      // format: on
      case IdentifyClient(ClientType.Personal, Service.Vat, basket) =>
        for {
          knownFactResult <- checkKnownFact(vatClient)
          endState <- knownFactResult match {
                        case kfcResponse @ (Pass | Fail(VatClientInsolvent)) =>
                          getClientName(vatClient.vrn.value, Service.Vat).flatMap { clientName =>
                            goto(
                              ConfirmClient(
                                AuthorisationRequest(clientName.getOrElse(""), Invitation(Some(ClientType.Personal), Service.Vat, vatClient.vrn)),
                                basket,
                                clientInsolvent = Some(kfcResponse == Fail(VatClientInsolvent))
                              )
                            )
                          }
                        case Fail(VatMigrationInProgress) => goto(CannotCreateRequest(basket))
                        case Fail(NotFound)               => goto(ClientNotSignedUp(Service.Vat, basket))
                        case Fail(NotMatched) | _         => goto(KnownFactNotMatched(basket))
                      }
        } yield endState

      case IdentifyClient(ClientType.Business, Service.Vat, basket) =>
        for {
          regDateMatches <- checkKnownFact(vatClient)
          endState <- regDateMatches match {
                        case kfcResponse @ (Pass | Fail(VatClientInsolvent)) =>
                          getClientName(vatClient.vrn.value, Service.Vat).flatMap { clientName =>
                            goto(
                              ConfirmClient(
                                AuthorisationRequest(clientName.getOrElse(""), Invitation(Some(ClientType.Business), Service.Vat, vatClient.vrn)),
                                basket,
                                clientInsolvent = Some(kfcResponse == Fail(VatClientInsolvent))
                              )
                            )

                          }
                        case Fail(VatMigrationInProgress) => goto(CannotCreateRequest(basket))
                        case Fail(NotFound)               => goto(ClientNotSignedUp(Service.Vat, Set.empty))
                        case Fail(NotMatched) | _         => goto(KnownFactNotMatched(basket))
                      }
        } yield endState
    }

    // format: off
    def identifiedIrvClient(agent: AuthorisedAgent)
                           (irvClient: IrvClient) = Transition {
      // format: on
      case IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord, basket) =>
        for {
          knownFactResult <- checkKnownFact(irvClient)

          endState <- knownFactResult match {
                        case Pass =>
                          getClientName(irvClient.nino.value, Service.PersonalIncomeRecord)
                            .map { clientName =>
                              AuthorisationRequest(
                                clientName.getOrElse(""),
                                Invitation(Some(ClientType.Personal), Service.PersonalIncomeRecord, irvClient.nino)
                              )
                            }
                            .flatMap { request =>
                              checkIfPendingOrActiveAndGoto(
                                ReviewAuthorisations(ClientType.Personal, Services.supportedServicesFor(ClientType.Personal), basket + request)
                              )(ClientType.Personal, agent.arn, request, Service.PersonalIncomeRecord, basket)
                            }
                        case Fail(NotFound)       => goto(KnownFactNotMatched(basket))
                        case Fail(NotMatched) | _ => goto(KnownFactNotMatched(basket))
                      }
        } yield endState
    }

    private def createAndProcessInvitations(
      clientType: ClientType,
      agencyEmail: String,
      invitationLink: String,
      someFailedState: Basket => State,
      basket: Basket,
      arn: Arn
    ) =
      for {
        processedRequests <- createMultipleInvitations(arn, basket)
        successState <- clientType match {
                          // separate handling for Personal as must look up whether it's an alt-ITSA invitation
                          case ClientType.Personal => createInvitationSent(agencyEmail, invitationLink, arn, basket)
                          case otherClientType =>
                            toFuture(InvitationSent(otherClientType, invitationLink, None, agencyEmail, basket.map(_.invitation.service)))
                        }
        result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests)) goto(successState)
                  else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                    goto(AllAuthorisationsFailed(processedRequests))
                  else goto(someFailedState(processedRequests))
      } yield result

    private def checkIfPendingOrActiveAndGoto(
      successState: State
    )(clientType: ClientType, arn: Arn, request: AuthorisationRequest, service: Service, basket: Basket): Future[State] =
      for {
        hasPendingInvitations <- if (
                                   basket.exists(_.invitation.service == service) &&
                                   basket.exists(_.invitation.clientId == request.invitation.clientId)
                                 )
                                   Future.successful(true)
                                 else
                                   hasPendingInvitationsFor(arn, request.invitation.clientId, service)
        result <- if (hasPendingInvitations) {
                    getAgentLink(arn, Some(clientType))
                      .flatMap(agentLink => goto(PendingInvitationExists(clientType, request.clientName, agentLink, basket)))
                  } else {
                    hasActiveRelationshipFor(arn, request.invitation.clientId, service).flatMap {
                      case true => goto(ActiveAuthorisationExists(clientType, service, basket))
                      case false =>
                        if (service == Service.MtdIt) hasPartialAuthorisationFor(arn, request.invitation.clientId).flatMap {
                          case true =>
                            goto(PartialAuthorisationExists(basket))
                          case false =>
                            if (appConfig.featuresAltItsa) {
                              legacySaRelationshipStatusFor(arn, request.invitation.clientId).flatMap {
                                case LegacySaRelationshipFoundAndMapped => goto(AlreadyCopiedAcrossItsa)
                                case LegacySaRelationshipFoundNotMapped => goto(LegacyAuthorisationDetected(basket + request))
                                case LegacySaRelationshipNotFound       => goto(successState)
                              }
                            } else goto(successState)
                        }
                        else goto(successState)
                    }
                  }
      } yield result

    /** User confirms that the client identified is the correct person. This transition may go to a review page (if there can be more than one item in
      * the basket) Otherwise it will just go straight to creation of the invitation and the what's next page
      */
    // format: off
    def clientConfirmed(authorisedAgent: AuthorisedAgent)
                       (confirmation: Confirmation) =
    // format: on
      Transition {

        case cc @ ConfirmClient(request, basket, _) if cc.service == Service.MtdIt =>
          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ReviewAuthorisations(ClientType.Personal, Services.supportedServicesFor(ClientType.Personal), basket + request)
            )(ClientType.Personal, authorisedAgent.arn, request, Service.MtdIt, basket)
          } else goto(IdentifyClient(ClientType.Personal, Service.MtdIt, basket))

        case cc @ ConfirmClient(request, basket, _) if cc.service == Service.CapitalGains =>
          require(
            Services.isSupported(request.invitation.clientType.get, request.invitation.service),
            "unexpected clientType/service combination: " + request.invitation
          )
          val Some(clientType) = request.invitation.clientType
          val reviewAuthState = ReviewAuthorisations(clientType, Services.supportedServicesFor(clientType), basket + request)

          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(reviewAuthState)(clientType, authorisedAgent.arn, request, Service.CapitalGains, basket)
          } else goto(IdentifyClient(clientType, Service.CapitalGains, basket))

        case cc @ ConfirmClient(request, basket, clientInsolvent) if cc.clientType.contains(ClientType.Personal) && cc.service == Service.Vat =>
          if (confirmation.choice) {
            if (clientInsolvent.contains(true)) goto(ClientInsolvent(basket))
            else
              checkIfPendingOrActiveAndGoto(
                ReviewAuthorisations(ClientType.Personal, Services.supportedServicesFor(ClientType.Personal), basket + request)
              )(ClientType.Personal, authorisedAgent.arn, request, Service.Vat, basket)
          } else goto(IdentifyClient(ClientType.Personal, Service.Vat, basket))

        case cc @ ConfirmClient(request, basket, isInsolvent) if cc.clientType.contains(ClientType.Business) && cc.service == Service.Vat =>
          if (isInsolvent.contains(true)) goto(ClientInsolvent(basket))
          else if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(
              ReviewAuthorisations(ClientType.Business, Services.supportedServicesFor(ClientType.Business), basket + request)
            )(ClientType.Business, authorisedAgent.arn, request, Service.Vat, basket)
          } else goto(IdentifyClient(ClientType.Business, Service.Vat, basket))

        case cc @ ConfirmClient(request, basket, _) if List(Service.Trust, Service.TrustNT).contains(cc.service) =>
          if (confirmation.choice) {
            if (featureFlags.isServiceEnabled(Service.CapitalGains))
              // if CGT is enabled, we need to go to the review page (since we are multi-select)
              checkIfPendingOrActiveAndGoto(
                ReviewAuthorisations(ClientType.Trust, Services.supportedServicesFor(ClientType.Trust), basket + request)
              )(ClientType.Trust, authorisedAgent.arn, request, request.invitation.service, basket)
            else
              // otherwise we go straight to create the invitation (no review necessary - only one service)
              for {
                hasPendingInvitations <- hasPendingInvitationsFor(authorisedAgent.arn, request.invitation.clientId, request.invitation.service)
                agentLink             <- getAgentLink(authorisedAgent.arn, Some(ClientType.Trust))
                result <- if (hasPendingInvitations) {
                            goto(PendingInvitationExists(ClientType.Trust, request.clientName, agentLink, Set.empty))
                          } else {
                            hasActiveRelationshipFor(authorisedAgent.arn, request.invitation.clientId, request.invitation.service)
                              .flatMap {
                                case true => goto(ActiveAuthorisationExists(ClientType.Trust, request.invitation.service, Set.empty))
                                case false =>
                                  getAgencyEmail().flatMap(agencyEmail =>
                                    createAndProcessInvitations(
                                      ClientType.Trust,
                                      agencyEmail,
                                      agentLink,
                                      (b: Basket) => SomeAuthorisationsFailed(agentLink, None, agencyEmail, b),
                                      Set(request),
                                      authorisedAgent.arn
                                    )
                                  )
                              }
                          }
              } yield result
          } else goto(IdentifyClient(ClientType.Trust, Service.Trust, basket))

        case cc @ ConfirmClient(request, basket, _) =>
          val (reviewAuthState, state, clientType) = cc.clientType match {
            case Some(clientType) =>
              (
                ReviewAuthorisations(clientType, Services.supportedServicesFor(clientType), basket + request),
                IdentifyClient(clientType, cc.service, basket),
                clientType
              )
            case None => throw new RuntimeException("unexpected clientType in the AuthorisationRequest")
          }

          if (confirmation.choice) {
            checkIfPendingOrActiveAndGoto(reviewAuthState)(clientType, authorisedAgent.arn, request, cc.service, basket)
          } else goto(state)

      }

    /** User confirms that they have legacy authorisation with the client. This transition may go to agent-mapping Or review authorisations as if from
      * confirm client
      */
    def confirmedLegacyAuthorisation(agent: AuthorisedAgent): Transition =
      Transition { case LegacyAuthorisationDetected(basket) =>
        goto(ReviewAuthorisations(ClientType.Personal, Services.supportedServicesFor(ClientType.Personal), basket))
      }

    def continueSomeResponsesFailed(agent: AuthorisedAgent) = Transition {
      case SomeAuthorisationsFailed(invitationLink, continueUrl, agencyEmail, basket) =>
        val services = basket.filter(_.state == AuthorisationRequest.CREATED).map(_.invitation.service)
        goto(InvitationSent(ClientType.Personal, invitationLink, continueUrl, agencyEmail, services, isAltItsa = None))
    }

    // format: off
    def authorisationsReviewed(agent: AuthorisedAgent)
                              (confirmation: Confirmation) =
      // format: on
      Transition { case ReviewAuthorisations(clientType, services, basket) =>
        if (confirmation.choice)
          goto(SelectService(clientType, services, basket))
        else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(clientType))
            result <- createAndProcessInvitations(
                        clientType,
                        agencyEmail,
                        invitationLink,
                        (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                        basket,
                        agent.arn
                      )
          } yield result
        }
      }

    def deleteAuthorisationRequest(itemId: String)(authorisedAgent: AuthorisedAgent) = {
      def findItem(basket: Basket): AuthorisationRequest =
        basket.find(_.itemId == itemId).getOrElse(throw new Exception("No Item to delete"))
      Transition { case ReviewAuthorisations(clientType, _, basket) =>
        goto(DeleteAuthorisationRequest(clientType, findItem(basket), basket))
      }
    }

    def confirmDeleteAuthorisationRequest(agent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition { case DeleteAuthorisationRequest(clientType, authorisationRequest, basket) =>
        val availableServices = Services.supportedServicesFor(clientType).filter(featureFlags.isServiceEnabled)
        if (confirmation.choice) {
          if ((basket - authorisationRequest).nonEmpty)
            goto(ReviewAuthorisations(clientType, availableServices, basket - authorisationRequest))
          else
            goto(AllAuthorisationsRemoved)
        } else {
          goto(ReviewAuthorisations(clientType, availableServices, basket))
        }
      }
  }
}
