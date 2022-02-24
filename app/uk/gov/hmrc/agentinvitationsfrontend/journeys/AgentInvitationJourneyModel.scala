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
import uk.gov.hmrc.agentmtdidentifiers.model.SuspensionDetails
import uk.gov.hmrc.agentinvitationsfrontend.config.AppConfig
import uk.gov.hmrc.agentinvitationsfrontend.controllers.FeatureFlags
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult.{VatDetailsNotFound, VatKnownFactCheckOk, VatKnownFactNotMatched, VatRecordClientInsolvent, VatRecordMigrationInProgress}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.util.toFuture
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.fsm.JourneyModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
    isAltItsa: Option[Boolean] = None)
      extends State

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
    type CheckPostcodeMatches = (Nino, String) => Future[Option[Boolean]]
    type CheckRegDateMatches = (Vrn, LocalDate) => Future[VatKnownFactCheckResult]
    type CreateMultipleInvitations =
      (Arn, Set[AuthorisationRequest]) => Future[Set[AuthorisationRequest]]
    type GetAgentLink = (Arn, Option[ClientType]) => Future[String]
    type GetAgencyEmail = () => Future[String]
    type GetTrustName = TrustTaxIdentifier => Future[TrustResponse]
    type GetCgtSubscription = CgtRef => Future[Option[CgtSubscription]]
    type GetPptSubscription = PptRef => Future[Option[PptSubscription]]
    type GetSuspensionDetails = () => Future[SuspensionDetails]
    type LegacySaRelationshipStatusFor = (Arn, String) => Future[LegacySaRelationshipResult]
    type CheckDOBMatches = (Nino, LocalDate) => Future[Option[Boolean]]
  }

  import TransitionEffects._

  case class Transitions(
    getSuspensionDetails: GetSuspensionDetails,
    hasPendingInvitationsFor: HasPendingInvitations,
    hasActiveRelationshipFor: HasActiveRelationship,
    hasPartialAuthorisationFor: HasPartialAuthorisation,
    legacySaRelationshipStatusFor: LegacySaRelationshipStatusFor,
    hasAltItsaInvitations: HasPartialAuthorisation,
    checkDobMatches: CheckDOBMatches,
    checkPostcodeMatches: CheckPostcodeMatches,
    checkRegDateMatches: CheckRegDateMatches,
    getClientName: GetClientName,
    getAgentLink: GetAgentLink,
    getAgencyEmail: GetAgencyEmail,
    createMultipleInvitations: CreateMultipleInvitations,
    createInvitationSent: CreateInvitationSent
  ) {
    val start: AgentInvitationJourneyModel.Transition = AgentInvitationJourneyModel.start

    def selectedClientType(agent: AuthorisedAgent)(clientType: String): Transition = Transition {
      case SelectClientType(basket) =>
        clientType match {
          case "personal" => goto(SelectService(ClientType.Personal, agent.personalServices, basket))
          case "business" => goto(SelectService(ClientType.Business, agent.businessServices, basket))
          case "trust"    => goto(SelectService(ClientType.Trust, agent.trustServices, basket))
        }
    }

    def gotoIdentify(
      serviceEnabled: Boolean,
      agentSuspensionEnabled: Boolean,
      arn: Arn,
      service: Service,
      identifyClientState: State,
      suspendedState: State): Future[State] =
      (serviceEnabled, agentSuspensionEnabled) match {
        case (true, true) =>
          getSuspensionDetails().flatMap { suspensionDetails =>
            if (suspensionDetails.isRegimeSuspended(service)) {
              goto(suspendedState)
            } else {
              goto(identifyClientState)
            }
          }
        case (true, false) => goto(identifyClientState)
        case (false, _)    => fail(new Exception(s"Service: ${service.id} feature flag is switched off"))
      }

    def selectedPersonalServiceMulti(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(service: Service) =
      selectedPersonalService(featureFlags)(agent)(Some(service))

    def selectedPersonalService(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(mService: Option[Service]) =
      Transition {

        case SelectService(ClientType.Personal, services, basket) =>
          mService match { // user selected "no" to final service
            case None => goto(ReviewAuthorisations(ClientType.Personal, services, basket))
            case Some(service) if services.contains(service) =>
              val flag = service match {
                case Service.MtdIt                => featureFlags.showHmrcMtdIt
                case Service.PersonalIncomeRecord => featureFlags.showPersonalIncome
                case Service.Vat                  => featureFlags.showHmrcMtdVat
                case Service.CapitalGains         => featureFlags.showHmrcCgt
                case Service.Ppt                  => featureFlags.showPlasticPackagingTax
              }
              gotoIdentify(
                flag,
                featureFlags.agentSuspensionEnabled,
                agent.arn,
                service,
                IdentifyClient(ClientType.Personal, service, basket),
                AgentSuspended(service, basket)
              )
            case _ => goto(SelectService(ClientType.Personal, services, basket))
          }
      }

    def selectedBusinessServiceMulti(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(service: Service) =
      selectedBusinessService(featureFlags)(agent)(Some(service))

    def selectedBusinessService(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(mService: Option[Service]) =
      Transition {
        case SelectService(ClientType.Business, services, basket) =>
          mService match {
            case None => // user selected "no" to final service
              if (basket.isEmpty)
                goto(SelectClientType(basket)) // if no services in basket, and user also declined the final service, go back to SelectClientType
              else goto(ReviewAuthorisations(ClientType.Business, services, basket)) // otherwise proceed to review
            case Some(service) if services.contains(service) =>
              val flag = service match {
                case Service.Vat => featureFlags.showHmrcMtdVat
                case Service.Ppt => featureFlags.showPlasticPackagingTax
              }
              gotoIdentify(
                flag,
                featureFlags.agentSuspensionEnabled,
                agent.arn,
                service,
                IdentifyClient(ClientType.Business, service, basket),
                AgentSuspended(service, basket)
              )
            case _ => goto(SelectService(ClientType.Business, services, basket))
          }
      }

    def selectedTrustServiceMulti(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(service: Service) =
      selectedTrustService(featureFlags)(agent)(Some(service))

    def selectedTrustService(featureFlags: FeatureFlags)(agent: AuthorisedAgent)(mService: Option[Service]) =
      Transition {
        case SelectService(ClientType.Trust, services, basket) =>
          mService match {
            case None => // user selected "no" to final service
              if (basket.nonEmpty)
                goto(ReviewAuthorisations(ClientType.Trust, services, basket))
              else {
                goto(root)
              }
            case Some(service) if services.contains(service) =>
              val flag = service match {
                case Service.Trust        => featureFlags.showHmrcTrust
                case Service.CapitalGains => featureFlags.showHmrcCgt
                case Service.Ppt          => featureFlags.showPlasticPackagingTax
              }
              gotoIdentify(
                flag,
                featureFlags.agentSuspensionEnabled,
                agent.arn,
                service,
                IdentifyClient(ClientType.Trust, service, basket),
                AgentSuspended(service, basket)
              )
            case _ => goto(SelectService(ClientType.Trust, services, basket))
          }
      }

    def identifiedTrustClient(getTrustName: GetTrustName)(agent: AuthorisedAgent)(trustClient: TrustClient) =
      Transition {
        case IdentifyClient(ClientType.Trust, trustService, basket) if List(Service.Trust, Service.TrustNT).contains(trustService) =>
          getTrustName(trustClient.taxId).flatMap { trustResponse =>
            trustResponse.response match {
              case Right(TrustName(name)) => {
                trustClient.taxId match {
                  case Utr(_) =>
                    goto(
                      ConfirmClient(
                        AuthorisationRequest(name, Invitation(Some(ClientType.Trust), trustService, Utr(trustClient.taxId.value))),
                        basket)
                    )
                  case Urn(_) =>
                    goto(
                      ConfirmClient(
                        AuthorisationRequest(name, Invitation(Some(ClientType.Trust), trustService, Urn(trustClient.taxId.value))),
                        basket)
                    )
                }

              }
              case Left(invalidTrust) =>
                logger.warn(s"Des returned $invalidTrust response for utr: ${trustClient.taxId}")
                goto(TrustNotFound(basket))
            }
          }
      }

    def identifyCgtClient(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(
      cgtClient: CgtClient): AgentInvitationJourneyModel.Transition = {
      def handle(showPostcode: CgtSubscription => State, showCountryCode: CgtSubscription => State, basket: Basket) =
        getCgtSubscription(cgtClient.cgtRef).map {
          case Some(subscription) =>
            if (subscription.isUKBasedClient) {
              showPostcode(subscription)
            } else {
              showCountryCode(subscription)
            }
          case None =>
            CgtRefNotFound(cgtClient.cgtRef, basket)
        }

      Transition {
        case IdentifyClient(ClientType.Trust, Service.CapitalGains, basket) =>
          handle(
            cgtSubscription => ConfirmPostcodeCgt(cgtClient.cgtRef, ClientType.Trust, basket, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription => ConfirmCountryCodeCgt(cgtClient.cgtRef, ClientType.Trust, basket, cgtSubscription.countryCode, cgtSubscription.name),
            basket
          )

        case IdentifyClient(ClientType.Personal, Service.CapitalGains, basket) =>
          handle(
            cgtSubscription => ConfirmPostcodeCgt(cgtClient.cgtRef, ClientType.Personal, basket, cgtSubscription.postCode, cgtSubscription.name),
            cgtSubscription =>
              ConfirmCountryCodeCgt(cgtClient.cgtRef, ClientType.Personal, basket, cgtSubscription.countryCode, cgtSubscription.name),
            basket
          )
      }
    }

    def identifyPptClient(checkKnownFact: PptClient => Future[Boolean], getPptCustomerName: PptRef => Future[Option[String]])(agent: AuthorisedAgent)(
      pptClient: PptClient): AgentInvitationJourneyModel.Transition = {
      def handle(mkState: String => State, basket: Basket) =
        for {
          isCheckOK     <- checkKnownFact(pptClient)
          mCustomerName <- if (isCheckOK) getPptCustomerName(pptClient.pptRef) else Future.successful(None)
        } yield {
          mCustomerName match {
            case Some(customerName) => mkState(customerName)
            case None               => PptRefNotFound(pptClient.pptRef, basket)
          }
        }

      Transition {
        case IdentifyClient(clientType, Service.Ppt, basket) =>
          handle(
            customerName => ConfirmClient(AuthorisationRequest(customerName, Invitation(Some(clientType), Service.Ppt, pptClient.pptRef)), basket),
            basket
          )
      }
    }

    private def removeSpaceFromPostcode(postcode: String): String =
      postcode.replace(" ", "")

    def confirmPostcodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(postcode: Postcode): Transition =
      Transition {
        case ConfirmPostcodeCgt(cgtRef, clientType, basket, postcodeFromDes, name) => {
          val userPostcodeWithoutSpace = removeSpaceFromPostcode(postcode.value)
          val desPostcodeWithoutSpace = removeSpaceFromPostcode(postcodeFromDes.getOrElse("no_des_postcode"))

          if (desPostcodeWithoutSpace == userPostcodeWithoutSpace) {
            goto(ConfirmClient(AuthorisationRequest(name, Invitation(Some(clientType), Service.CapitalGains, cgtRef)), basket))
          } else {
            logger.warn(s"CGT postcode match failed. DES postcode was ${postcodeFromDes
              .getOrElse("not found")} and user entered ${postcode.value}")
            goto(KnownFactNotMatched(basket))
          }
        }
      }

    def confirmCountryCodeCgt(getCgtSubscription: GetCgtSubscription)(agent: AuthorisedAgent)(countryCode: CountryCode): Transition =
      Transition {
        case ConfirmCountryCodeCgt(cgtRef, clientType, basket, countryCodeFromDes, name) =>
          if (countryCodeFromDes.contains(countryCode.value)) {
            goto(ConfirmClient(AuthorisationRequest(name, Invitation(Some(clientType), Service.CapitalGains, cgtRef)), basket))
          } else {
            goto(KnownFactNotMatched(basket))
          }
      }

    // format: off
    def identifiedItsaClient(appConfig: AppConfig)
                            (agent: AuthorisedAgent)
                            (itsaClient: ItsaClient) = Transition {
      // format: on
      case IdentifyClient(ClientType.Personal, Service.MtdIt, basket) =>
        for {
          postcodeMatches <- checkPostcodeMatches(Nino(itsaClient.clientIdentifier), itsaClient.postcode)
          endState <- postcodeMatches match {
                       case Some(true) =>
                         getClientName(itsaClient.clientIdentifier, Service.MtdIt).flatMap { clientName =>
                           goto(
                             ConfirmClient(
                               AuthorisationRequest(
                                 clientName.getOrElse(""),
                                 Invitation(Some(ClientType.Personal), Service.MtdIt, Nino(itsaClient.clientIdentifier))),
                               basket))
                         }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None =>
                         if (appConfig.featuresAltItsa) goto(ClientNotRegistered(basket)) else goto(ClientNotSignedUp(Service.MtdIt, basket))
                     }
        } yield endState
    }

    // format: off
    def identifiedVatClient(agent: AuthorisedAgent)
                           (vatClient: VatClient) = Transition {
      // format: on
      case IdentifyClient(ClientType.Personal, Service.Vat, basket) =>
        for {
          regDateMatches <- checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
          endState <- regDateMatches match {
                       case kfcResponse @ (VatKnownFactCheckOk | VatRecordClientInsolvent) =>
                         getClientName(vatClient.clientIdentifier, Service.Vat).flatMap { clientName =>
                           goto(
                             ConfirmClient(
                               AuthorisationRequest(
                                 clientName.getOrElse(""),
                                 Invitation(Some(ClientType.Personal), Service.Vat, Vrn(vatClient.clientIdentifier))),
                               basket,
                               clientInsolvent = Some(kfcResponse == VatRecordClientInsolvent)
                             ))
                         }
                       case VatRecordMigrationInProgress => goto(CannotCreateRequest(basket))
                       case VatKnownFactNotMatched       => goto(KnownFactNotMatched(basket))
                       case VatDetailsNotFound           => goto(ClientNotSignedUp(Service.Vat, basket))
                     }
        } yield endState

      case IdentifyClient(ClientType.Business, service, basket) =>
        for {
          regDateMatches <- checkRegDateMatches(Vrn(vatClient.clientIdentifier), LocalDate.parse(vatClient.registrationDate))
          endState <- regDateMatches match {
                       case kfcResponse @ (VatKnownFactCheckOk | VatRecordClientInsolvent) =>
                         getClientName(vatClient.clientIdentifier, Service.Vat).flatMap { clientName =>
                           goto(
                             ConfirmClient(
                               AuthorisationRequest(
                                 clientName.getOrElse(""),
                                 Invitation(Some(ClientType.Business), Service.Vat, Vrn(vatClient.clientIdentifier))),
                               basket,
                               clientInsolvent = Some(kfcResponse == VatRecordClientInsolvent)
                             )
                           )

                         }
                       case VatRecordMigrationInProgress => goto(CannotCreateRequest(basket))
                       case VatKnownFactNotMatched       => goto(KnownFactNotMatched(basket))
                       case VatDetailsNotFound           => goto(ClientNotSignedUp(Service.Vat, Set.empty))
                     }
        } yield endState
    }

    // format: off
    def identifiedIrvClient(appConfig: AppConfig)
                           (agent: AuthorisedAgent)
                           (irvClient: IrvClient) = Transition {
      // format: on
      case IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord, basket) =>
        for {
          dobMatches <- checkDobMatches(Nino(irvClient.clientIdentifier), LocalDate.parse(irvClient.dob))

          endState <- dobMatches match {
                       case Some(true) =>
                         getClientName(irvClient.clientIdentifier, Service.PersonalIncomeRecord)
                           .map { clientName =>
                             AuthorisationRequest(
                               clientName.getOrElse(""),
                               Invitation(Some(ClientType.Personal), Service.PersonalIncomeRecord, Nino(irvClient.clientIdentifier)))
                           }
                           .flatMap { request =>
                             checkIfPendingOrActiveAndGoto(ReviewAuthorisations(ClientType.Personal, agent.personalServices, basket + request))(
                               ClientType.Personal,
                               agent.arn,
                               request,
                               Service.PersonalIncomeRecord,
                               basket)(appConfig)
                           }
                       case Some(false) => goto(KnownFactNotMatched(basket))
                       case None        => goto(KnownFactNotMatched(basket))
                     }
        } yield endState
    }

    private def createAndProcessInvitations(
      clientType: ClientType,
      agencyEmail: String,
      invitationLink: String,
      someFailedState: Basket => State,
      basket: Basket,
      arn: Arn) =
      for {
        processedRequests <- createMultipleInvitations(arn, basket)
        successState <- clientType match {
                         // separate handling for Personal as must look up whether it's an alt-ITSA invitation
                         case ClientType.Personal => createInvitationSent(agencyEmail, invitationLink, arn, basket)
                         case ClientType.Business =>
                           toFuture(InvitationSent(ClientType.Business, invitationLink, None, agencyEmail, basket.map(_.invitation.service)))
                         case ClientType.Trust =>
                           toFuture(InvitationSent(ClientType.Trust, invitationLink, None, agencyEmail, basket.map(_.invitation.service)))
                       }
        result <- if (AuthorisationRequest.eachHasBeenCreatedIn(processedRequests)) goto(successState)
                 else if (AuthorisationRequest.noneHaveBeenCreatedIn(processedRequests))
                   goto(AllAuthorisationsFailed(processedRequests))
                 else goto(someFailedState(processedRequests))
      } yield result

    private def checkIfPendingOrActiveAndGoto(
      successState: State)(clientType: ClientType, arn: Arn, request: AuthorisationRequest, service: Service, basket: Basket)(
      appConfig: AppConfig): Future[State] =
      for {
        hasPendingInvitations <- if (basket.exists(_.invitation.service == service) &&
                                     basket.exists(_.invitation.clientId == request.invitation.clientId))
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
                       } else goto(successState)
                   }
                 }
      } yield result

    /** User confirms that the client identified is the correct person.
      *  This transition may go to a review page (if there can be more than one item in the basket)
      *  Otherwise it will just go straight to creation of the invitation and the what's next page
      *
      * */
    // format: off
    def clientConfirmed(showCgtFlag: Boolean)
                       (appConfig: AppConfig)
                       (authorisedAgent: AuthorisedAgent)
                       (confirmation: Confirmation) =
    // format: on
    Transition {

      case cc @ ConfirmClient(request, basket, _) if cc.service == Service.MtdIt =>
        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket + request))(
            ClientType.Personal,
            authorisedAgent.arn,
            request,
            Service.MtdIt,
            basket)(appConfig)
        } else goto(IdentifyClient(ClientType.Personal, Service.MtdIt, basket))

      case cc @ ConfirmClient(request, basket, _) if cc.service == Service.CapitalGains => {
        val (reviewAuthState, state, clientType) = request.invitation.clientType match {
          case Some(ClientType.Trust) =>
            (
              ReviewAuthorisations(ClientType.Trust, authorisedAgent.trustServices, basket + request),
              IdentifyClient(ClientType.Trust, Service.CapitalGains, basket),
              ClientType.Trust)
          case Some(ClientType.Personal) =>
            (
              ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket + request),
              IdentifyClient(ClientType.Personal, Service.CapitalGains, basket),
              ClientType.Personal)
          case x => throw new RuntimeException("unexpected clientType in the AuthorisationRequest: " + x) //TODO
        }

        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(reviewAuthState)(clientType, authorisedAgent.arn, request, Service.CapitalGains, basket)(appConfig)
        } else goto(state)
      }

      case cc @ ConfirmClient(request, basket, clientInsolvent) if cc.clientType.contains(ClientType.Personal) && cc.service == Service.Vat =>
        if (confirmation.choice) {
          if (clientInsolvent.contains(true)) goto(ClientInsolvent(basket))
          else
            checkIfPendingOrActiveAndGoto(ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket + request))(
              ClientType.Personal,
              authorisedAgent.arn,
              request,
              Service.Vat,
              basket)(appConfig)
        } else goto(IdentifyClient(ClientType.Personal, Service.Vat, basket))

      case cc @ ConfirmClient(request, basket, isInsolvent) if cc.clientType.contains(ClientType.Business) && cc.service == Service.Vat =>
        if (isInsolvent.contains(true)) goto(ClientInsolvent(basket))
        else if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(ReviewAuthorisations(ClientType.Business, authorisedAgent.businessServices, basket + request))(
            ClientType.Business,
            authorisedAgent.arn,
            request,
            Service.Vat,
            basket)(appConfig)
        } else goto(IdentifyClient(ClientType.Business, Service.Vat, basket))

      case cc @ ConfirmClient(request, basket, _) if List(Service.Trust, Service.TrustNT).contains(cc.service) =>
        if (confirmation.choice) {
          if (showCgtFlag)
            // if CGT is enabled, we need to go to the review page (since we are multi-select)
            checkIfPendingOrActiveAndGoto(ReviewAuthorisations(ClientType.Trust, authorisedAgent.trustServices, basket + request))(
              ClientType.Trust,
              authorisedAgent.arn,
              request,
              request.invitation.service,
              basket)(appConfig)
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
                               getAgencyEmail().flatMap(
                                 agencyEmail =>
                                   createAndProcessInvitations(
                                     ClientType.Trust,
                                     agencyEmail,
                                     agentLink,
                                     (b: Basket) => SomeAuthorisationsFailed(agentLink, None, agencyEmail, b),
                                     Set(request),
                                     authorisedAgent.arn
                                 ))
                           }
                       }
            } yield result
        } else goto(IdentifyClient(ClientType.Trust, Service.Trust, basket))

      case cc @ ConfirmClient(request, basket, _) if cc.service == Service.Ppt => {
        val (reviewAuthState, state, clientType) = cc.clientType match {
          case Some(ClientType.Trust) =>
            (
              ReviewAuthorisations(ClientType.Trust, authorisedAgent.trustServices, basket + request),
              IdentifyClient(ClientType.Trust, Service.Ppt, basket),
              ClientType.Trust)
          case Some(ClientType.Personal) =>
            (
              ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket + request),
              IdentifyClient(ClientType.Personal, Service.Ppt, basket),
              ClientType.Personal)
          case Some(ClientType.Business) =>
            (
              ReviewAuthorisations(ClientType.Business, authorisedAgent.businessServices, basket + request),
              IdentifyClient(ClientType.Business, Service.Ppt, basket),
              ClientType.Business)
          case None => throw new RuntimeException("unexpected clientType in the AuthorisationRequest")
        }

        if (confirmation.choice) {
          checkIfPendingOrActiveAndGoto(reviewAuthState)(clientType, authorisedAgent.arn, request, Service.Ppt, basket)(appConfig)
        } else goto(state)
      }

    }

    /** User confirms that they have legacy authorisation with the client.
      *  This transition may go to agent-mapping
      *  Or review authorisations as if from confirm client
      * */
    def confirmedLegacyAuthorisation(authorisedAgent: AuthorisedAgent): Transition =
      Transition {
        case LegacyAuthorisationDetected(basket) =>
          goto(ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket))
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
    Transition {
      case ReviewAuthorisations(ClientType.Trust, _, basket) =>
        if (confirmation.choice)
          goto(SelectService(ClientType.Trust, agent.trustServices, basket))
        else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(ClientType.Trust))
            services = basket.map(_.invitation.service.id)
            result <- createAndProcessInvitations(
                       ClientType.Trust,
                       agencyEmail,
                       invitationLink,
                       (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                       basket,
                       agent.arn
                     )
          } yield result
        }

      case ReviewAuthorisations(ClientType.Personal, _, basket) =>
        if (confirmation.choice) {
          goto(SelectService(ClientType.Personal, agent.personalServices, basket))
        } else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(ClientType.Personal))
            result <- createAndProcessInvitations(
                       ClientType.Personal,
                       agencyEmail,
                       invitationLink,
                       (b: Basket) => SomeAuthorisationsFailed(invitationLink, None, agencyEmail, b),
                       basket,
                       agent.arn
                     )
          } yield result
        }

      case ReviewAuthorisations(ClientType.Business, _, basket) =>
        if (confirmation.choice) {
          goto(SelectService(ClientType.Business, agent.businessServices, basket))
        } else {
          for {
            agencyEmail    <- getAgencyEmail()
            invitationLink <- getAgentLink(agent.arn, Some(ClientType.Business))
            result <- createAndProcessInvitations(
                       ClientType.Business,
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
      Transition {
        case ReviewAuthorisations(clientType, _, basket) =>
          goto(DeleteAuthorisationRequest(clientType, findItem(basket), basket))
      }
    }

    def confirmDeleteAuthorisationRequest(authorisedAgent: AuthorisedAgent)(confirmation: Confirmation) =
      Transition {

        case DeleteAuthorisationRequest(ClientType.Personal, authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisations(ClientType.Personal, authorisedAgent.personalServices, basket))
          }

        case DeleteAuthorisationRequest(ClientType.Business, authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisations(ClientType.Business, authorisedAgent.businessServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisations(ClientType.Business, authorisedAgent.businessServices, basket))
          }

        case DeleteAuthorisationRequest(ClientType.Trust, authorisationRequest, basket) =>
          if (confirmation.choice) {
            if ((basket - authorisationRequest).nonEmpty)
              goto(ReviewAuthorisations(ClientType.Trust, authorisedAgent.trustServices, basket - authorisationRequest))
            else
              goto(AllAuthorisationsRemoved)
          } else {
            goto(ReviewAuthorisations(ClientType.Trust, authorisedAgent.trustServices, basket))
          }
      }
  }
}
