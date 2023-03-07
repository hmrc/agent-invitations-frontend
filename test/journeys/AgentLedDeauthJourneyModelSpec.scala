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

package journeys

import java.time.LocalDate
import play.api.test.Helpers._
import support.{TestFeatureFlags, UnitSpec}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.TransitionEffects._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.TransitionEffects._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel._
import uk.gov.hmrc.agentinvitationsfrontend.journeys._
import uk.gov.hmrc.agentinvitationsfrontend.models.VatKnownFactCheckResult._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentmtdidentifiers.model._
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AgentLedDeauthJourneyModelSpec extends UnitSpec with StateMatchers[State] {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  case class given(initialState: State) extends AgentLedDeauthJourneyService with TestStorage[(State, List[State])] {
    await(save((initialState, Nil)))

    def when(transition: Transition): (State, List[State]) =
      await(super.apply(transition))
  }

  val authorisedAgent: AuthorisedAgent = AuthorisedAgent(Arn("TARN0000001"))
  val postCode = "BN114AW"
  val vrn = "123456"
  val nino = "AB123456A"
  val vatRegDate = "2010-10-10"
  val dob = "1990-10-10"

  val utr = Utr("1977030537")
  val TrustNotFoundResponse = TrustResponse(Left(InvalidTrust("RESOURCE_NOT_FOUND", "blah")))
  val cgtRef = CgtRef("XMCGTP123456789")
  val pptRef = PptRef("XAPPT0000012345")

  val tpd = TypeOfPersonDetails("Individual", Left(IndividualName("firstName", "lastName")))

  def anIdentifierFor(service: Service): TaxIdentifier = service match {
    case Service.MtdIt                => Nino(nino)
    case Service.PersonalIncomeRecord => Nino(nino)
    case Service.Vat                  => Vrn(vrn)
    case Service.Trust                => utr
    case Service.CapitalGains         => cgtRef
    case Service.Ppt                  => pptRef
  }

  def getClientName(clientId: String, service: Service) = Future(Some("John Smith"))

  def hasNoPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(false)

  def hasPendingInvitation(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(true)

  def hasNoActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(false)

  def hasActiveRelationship(arn: Arn, clientId: String, service: Service): Future[Boolean] =
    Future.successful(true)

  def hasNoPartialAuthorisation(arn: Arn, clientId: String): Future[Boolean] =
    Future successful (false)

  def cgtAddressDetails(countryCode: String = "GB") =
    CgtAddressDetails("line1", Some("line2"), Some("line2"), Some("line2"), countryCode, Some("BN13 1FN"))

  def cgtSubscription(countryCode: String = "GB") =
    CgtSubscription("CGT", SubscriptionDetails(tpd, cgtAddressDetails(countryCode)))

  def getAgencyName: GetAgencyName = _ => Future("Popeye")

  def getAgencyEmail: GetAgencyEmail = () => Future("abc@xyz.com")

  def getCgtSubscription(countryCode: String = "GB"): GetCgtSubscription = { cgtRef: CgtRef =>
    Future.successful(Some(cgtSubscription(countryCode)))
  }

  def deleteRelationshipSucceeds: DeleteRelationship = (_, _, _) => Future.successful(Some(true))
  def deleteRelationshipFails: DeleteRelationship = (_, _, _) => Future.successful(Some(false))
  def setRelationshipEndedSucceeds: SetRelationshipEnded = (_, _, _) => Future.successful(Some(true))

  def vatCheckReturns(result: VatKnownFactCheckResult): VatRegDateMatches = (_, _) => Future.successful(result)

  // This is the default behaviour. Modify as needed in individual tests
  val transitions = Transitions(
    TestFeatureFlags.allEnabled,
    checkPostcodeMatches = (_, _) => Future.successful(None),
    hasActiveRelationshipFor = hasNoActiveRelationship,
    hasPartialAuthorisationFor = hasNoPartialAuthorisation,
    getClientName = getClientName,
    checkDOBMatches = (_, _) => Future.successful(None),
    checkVatRegDateMatches = vatCheckReturns(VatDetailsNotFound),
    deleteRelationship = deleteRelationshipSucceeds,
    setRelationshipEnded = setRelationshipEndedSucceeds,
    getAgencyName = getAgencyName,
    getCgtSubscription = _ => Future(None),
    getPptSubscription = _ => Future(None),
    getTrustName = _ => Future(TrustResponse(Right(TrustName("some-trust"))))
  )

  val transitionsWithActiveRelationship = transitions.copy(hasActiveRelationshipFor = hasActiveRelationship)

  "AgentLedDeauthJourneyModel" when {
    "at state ClientType" should {
      ClientType.clientTypes.foreach { clientType =>
        s"transition to SelectService ($clientType) when $clientType is selected" in {
          given(SelectClientType) when transitions.selectedClientType(authorisedAgent)(ClientType.fromEnum(clientType)) should thenGo(
            SelectService(clientType, Services.supportedServicesFor(clientType)))
        }
      }
    }

    ClientType.clientTypes.foreach { clientType =>
      s"at state SelectService ($clientType)" should {
        val availableServices = Services.supportedServicesFor(clientType)
        availableServices.foreach { service =>
          s"throw an exception when service is $service and the corresponding flag is switched off" in {
            intercept[Exception] {
              val flags = TestFeatureFlags.allEnabled.disable(service)
              given(SelectService(clientType, Services.supportedServicesFor(clientType))) when transitions
                .copy(featureFlags = flags)
                .chosenServiceMulti(authorisedAgent)(service)
            }.getMessage shouldBe s"Service: $service is not enabled"
          }
          s"transition to IdentifyClient ($clientType) when service is $service and feature flag is on" in {
            given(SelectService(clientType, Services.supportedServicesFor(clientType))) when transitions
              .copy(featureFlags = TestFeatureFlags.allEnabled)
              .chosenServiceMulti(authorisedAgent)(service) should thenGo(
              IdentifyClient(clientType, service)
            )
          }
        }
        val singleService = availableServices.head
        val singleServiceFeatureFlags = (availableServices - singleService).foldLeft(TestFeatureFlags.allEnabled)(_.disable(_)) // all services disabled except the first
        s"(when only one service is available) transition to IdentifyClient ($clientType) when YES is selected" in {
          given(SelectService(clientType, Set(singleService))) when
            transitions.copy(featureFlags = singleServiceFeatureFlags).chosenService(authorisedAgent)(Some(singleService)) should
            thenGo(IdentifyClient(clientType, singleService))
        }
        "(when only one service is available) transition to ClientType when NO is selected" in {
          given(SelectService(clientType, Set(singleService))) when
            transitions.copy(featureFlags = singleServiceFeatureFlags).chosenService(authorisedAgent)(None) should
            thenGo(SelectClientType)
        }
        "(when only one service is available) throw an exception when YES is selected but the service is not enabled" in {
          intercept[Exception] {
            given(SelectService(clientType, Set(singleService))) when
              transitions.copy(featureFlags = singleServiceFeatureFlags.disable(singleService)).chosenService(authorisedAgent)(Some(singleService))
          }.getMessage shouldBe s"Service: $singleService is not enabled"
        }
      }
    }

    "at state IdentifyClientPersonal" should {
      val itsaClient = ItsaClient(nino, postCode)
      val irvClient = IrvClient(nino, dob)
      val vatClient = VatClient(vrn, vatRegDate)
      val cgtClient = CgtClient(cgtRef)

      "transition to ConfirmClientItsa when postcode matches" in {
        def postcodeMatches(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClient(ClientType.Personal, Service.MtdIt)) when transitionsWithActiveRelationship
          .copy(checkPostcodeMatches = postcodeMatches)
          .submitIdentifyClientItsa(authorisedAgent)(itsaClient) should thenGo(
          ConfirmClient(ClientType.Personal, Service.MtdIt, Some("John Smith"), Nino(nino)))
      }
      "transition to KnownFactNotMatched when postcode does not match" in {
        def postcodeDoesNotMatch(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClient(ClientType.Personal, Service.MtdIt)) when transitionsWithActiveRelationship
          .copy(checkPostcodeMatches = postcodeDoesNotMatch)
          .submitIdentifyClientItsa(authorisedAgent)(itsaClient) should thenGo(KnownFactNotMatched)
      }
      "transition to NotSignedUp when client is not enrolled for itsa" in {
        def clientNotSignedUp(nino: Nino, postcode: String): Future[Option[Boolean]] = Future(None)

        given(IdentifyClient(ClientType.Personal, Service.MtdIt)) when transitionsWithActiveRelationship
          .copy(checkPostcodeMatches = clientNotSignedUp)
          .submitIdentifyClientItsa(authorisedAgent)(itsaClient) should thenGo(NotSignedUp(Service.MtdIt))
      }
      "throw an Exception when the client has no postcode" in {
        def postcodeDoesNotMatch(nino: Nino, postcode: String): Future[Some[Boolean]] = Future(Some(false))

        intercept[Exception] {
          given(IdentifyClient(ClientType.Personal, Service.MtdIt)) when transitionsWithActiveRelationship
            .copy(checkPostcodeMatches = postcodeDoesNotMatch)
            .submitIdentifyClientItsa(authorisedAgent)(ItsaClient(nino, ""))
        }.getMessage shouldBe "Postcode expected but none found"
      }
      "transition to ConfirmCancel when dob matches" in {
        def dobMatches(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(true))

        given(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord)) when transitionsWithActiveRelationship
          .copy(checkDOBMatches = dobMatches)
          .submitIdentifyClientIrv(authorisedAgent)(irvClient) should thenGo(ConfirmCancel(Service.PersonalIncomeRecord, Some("John Smith"), nino))
      }
      "transition to KnownFactNotMatched when dob does not match" in {
        def dobDoesNotMatch(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        given(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord)) when transitionsWithActiveRelationship
          .copy(checkDOBMatches = dobDoesNotMatch)
          .submitIdentifyClientIrv(authorisedAgent)(irvClient) should thenGo(KnownFactNotMatched)
      }
      "transition to NotSignedUp when client endpoint returns None" in {
        def clientNotSignedUp(nino: Nino, localDate: LocalDate): Future[Option[Boolean]] = Future(None)

        given(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord)) when transitionsWithActiveRelationship
          .copy(checkDOBMatches = clientNotSignedUp)
          .submitIdentifyClientIrv(authorisedAgent)(irvClient) should thenGo(NotSignedUp(Service.PersonalIncomeRecord))
      }
      "throw an Exception when the client has no dob" in {
        def dobDoesNotMatch(nino: Nino, localDate: LocalDate): Future[Some[Boolean]] = Future(Some(false))

        intercept[Exception] {
          given(IdentifyClient(ClientType.Personal, Service.PersonalIncomeRecord)) when transitionsWithActiveRelationship
            .copy(checkDOBMatches = dobDoesNotMatch)
            .submitIdentifyClientIrv(authorisedAgent)(IrvClient(nino, ""))
        }.getMessage shouldBe "Date of birth expected but none found"
      }
      "transition to ConfirmClientVat when vat reg date matches" in {
        given(IdentifyClient(ClientType.Personal, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatKnownFactCheckOk))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(
          ConfirmClient(ClientType.Personal, Service.Vat, Some("John Smith"), Vrn(vrn)))
      }
      "transition to KnownFactNotMatched when vat reg date does not match" in {
        given(IdentifyClient(ClientType.Personal, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatKnownFactNotMatched))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(KnownFactNotMatched)
      }

      "transition to ConfirmClientPersonalVat when vat reg date matches but client is insolvent" in {
        given(IdentifyClient(ClientType.Personal, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatRecordClientInsolvent))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(
          ConfirmClient(ClientType.Personal, Service.Vat, Some("John Smith"), Vrn("123456")))
      }

      "transition to Not signed up when vat record is being migrated" in {
        given(IdentifyClient(ClientType.Personal, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatRecordMigrationInProgress))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(NotSignedUp(Service.Vat))
      }

      "transition to NotSignedUp when client is not enrolled for VAT" in {
        given(IdentifyClient(ClientType.Personal, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatDetailsNotFound))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(NotSignedUp(Service.Vat))
      }
      "throw an Exception when the client has no vat reg date" in {
        intercept[Exception] {
          given(IdentifyClient(ClientType.Personal, Service.Vat)) when transitionsWithActiveRelationship
            .copy(checkVatRegDateMatches = vatCheckReturns(VatDetailsNotFound))
            .submitIdentifyClientVat(authorisedAgent)(VatClient(vrn, ""))
        }.getMessage shouldBe "Vat registration date expected but none found"
      }

      "transition to ConfirmPostcodeCgt for cgt for UK based clients" in {
        given(IdentifyClient(ClientType.Personal, Service.CapitalGains)) when transitionsWithActiveRelationship
          .copy(getCgtSubscription = getCgtSubscription())
          .submitIdentifyClientCgt(authorisedAgent)(cgtClient) should thenGo(
          ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "firstName lastName"))
      }

      "transition to ConfirmCountryCodeCgt for cgt for non-UK based clients" in {
        given(IdentifyClient(ClientType.Personal, Service.CapitalGains)) when transitionsWithActiveRelationship
          .copy(getCgtSubscription = getCgtSubscription("FR"))
          .submitIdentifyClientCgt(authorisedAgent)(cgtClient) should thenGo(
          ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "FR", "firstName lastName"))
      }
    }

    "at state IdentifyClientBusiness" should {
      val vatClient = VatClient(vrn, vatRegDate)
      "transition to ConfirmClientVat when known fact matches" in {
        given(IdentifyClient(ClientType.Business, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatKnownFactCheckOk))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(
          ConfirmClient(ClientType.Business, Service.Vat, Some("John Smith"), Vrn(vrn)))
      }
      "transition to KnownFactNotMatched when known fact does not match" in {
        given(IdentifyClient(ClientType.Business, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatKnownFactNotMatched))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(KnownFactNotMatched)
      }

      "transition to NotSignedUp when client is not enrolled" in {
        given(IdentifyClient(ClientType.Business, Service.Vat)) when transitionsWithActiveRelationship
          .copy(checkVatRegDateMatches = vatCheckReturns(VatDetailsNotFound))
          .submitIdentifyClientVat(authorisedAgent)(vatClient) should thenGo(NotSignedUp(Service.Vat))
      }
    }

    "at state IdentifyClientTrust" should {
      val trustClient = TrustClient(utr)
      "transition to ConfirmClientTrust when a trust is found for a given utr" in {
        given(IdentifyClient(ClientType.Trust, Service.Trust)) when transitions.submitIdentifyClientTrust(authorisedAgent)(trustClient) should thenGo(
          ConfirmClient(ClientType.Trust, Service.Trust, Some("some-trust"), utr))
      }
      "transition to TrustNotFound when a trust is not found for a given utr" in {
        given(IdentifyClient(ClientType.Trust, Service.Trust)) when transitions
          .copy(getTrustName = _ => Future(TrustNotFoundResponse))
          .submitIdentifyClientTrust(authorisedAgent)(trustClient) should thenGo(TrustNotFound)
      }
    }

    "at state ConfirmPostcodeCgt" should {
      "transition to ConfirmClientCgt for cgt" in {
        given(ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "firstName lastName")) when transitions.confirmPostcodeCgt(
          authorisedAgent)(Postcode("BN13 1FN")) should thenGo(
          ConfirmClient(ClientType.Personal, Service.CapitalGains, Some("firstName lastName"), cgtRef))
      }

      "transition to KnownFactNotMatched if postcodes do not match" in {
        given(ConfirmPostcodeCgt(ClientType.Personal, cgtRef, Some("BN13 1FN"), "firstName lastName")) when transitions.confirmPostcodeCgt(
          authorisedAgent)(Postcode("AAA")) should thenGo(KnownFactNotMatched)
      }
    }

    "at state ConfirmCountryCodeCgt" should {
      "transition to ConfirmClientCgt for cgt" in {
        given(ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "FR", "firstName lastName")) when transitions.confirmCountryCodeCgt(authorisedAgent)(
          CountryCode("FR")) should thenGo(ConfirmClient(ClientType.Personal, Service.CapitalGains, Some("firstName lastName"), cgtRef))
      }

      "transition to KnownFactNotMatched if country codes do not match" in {
        given(ConfirmCountryCodeCgt(ClientType.Personal, cgtRef, "FR", "firstName lastName")) when transitions.confirmCountryCodeCgt(authorisedAgent)(
          CountryCode("FRX")) should thenGo(KnownFactNotMatched)
      }
    }

    ClientType.clientTypes.foreach { clientType =>
      val availableServices = Services.supportedServicesFor(clientType)
      availableServices.foreach { service =>
        val taxId = anIdentifierFor(service)
        s"at state ConfirmClient ($clientType, $service)" should {
          "transition to ConfirmCancel when YES is selected" in {
            given(ConfirmClient(clientType, service, Some("client-name"), taxId)) when
              transitionsWithActiveRelationship.clientConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
              ConfirmCancel(service, Some("client-name"), taxId.value)
            )
          }
          "transition to root when NO is selected" in {
            given(ConfirmClient(clientType, service, Some("client-name"), taxId)) when
              transitionsWithActiveRelationship.clientConfirmed(authorisedAgent)(Confirmation(false)) should thenGo(
              SelectClientType
            )
          }
          "transition to NotAuthorised when there are no active relationships" in {
            given(ConfirmClient(clientType, service, Some("client-name"), taxId)) when
              transitions.clientConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
              NotAuthorised(service)
            )
          }
          if (clientType == ClientType.Personal && service == Service.MtdIt) {
            "transition to ConfirmCancel when there is only a partial auth (auth-itsa) and YES is selected" in {
              given(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Lucy Rose"), taxId)) when
                transitions
                  .copy(hasPartialAuthorisationFor = (_, _) => Future(true))
                  .clientConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
                ConfirmCancel(Service.MtdIt, Some("Lucy Rose"), nino, true)
              )
            }
            "transition to root when there is only a partial auth (auth-itsa) and NO is selected" in {
              given(ConfirmClient(ClientType.Personal, Service.MtdIt, Some("Lucy Rose"), taxId)) when
                transitions
                  .copy(hasPartialAuthorisationFor = (_, _) => Future(true))
                  .clientConfirmed(authorisedAgent)(Confirmation(false)) should thenGo(
                SelectClientType
              )
            }
          }
        }
      }
    }

    "at state ConfirmCancel" should {
      "transition to AuthorisationCancelled when YES is selected" in {
        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino)) when transitions.cancelConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(Service.MtdIt, Some("Holly Herndon"), "Popeye")
        )
      }

      "transition to AuthorisationCancelled when YES is selected for alt-itsa" in {
        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino, true)) when transitions.cancelConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(Service.MtdIt, Some("Holly Herndon"), "Popeye")
        )
      }

      "transition to AuthorisationCancelled when YES is selected and service is Trust" in {
        given(ConfirmCancel(Service.Trust, Some("some-trust"), utr.value)) when transitions.cancelConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
          AuthorisationCancelled(Service.Trust, Some("some-trust"), "Popeye")
        )
      }

      "transition to select client type when NO is selected" in {
        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino)) when transitions.cancelConfirmed(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }

      "transition to select client type when NO is selected for alt-itsa" in {
        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino, true)) when transitions.cancelConfirmed(authorisedAgent)(Confirmation(false)) should thenGo(
          SelectClientType
        )
      }
      "transition to ResponseFailed when the relationship termination fails" in {
        given(ConfirmCancel(Service.MtdIt, Some("Holly Herndon"), nino)) when transitions
          .copy(deleteRelationship = deleteRelationshipFails)
          .cancelConfirmed(authorisedAgent)(Confirmation(true)) should thenGo(
          ResponseFailed(Service.MtdIt, Some("Holly Herndon"), nino)
        )
      }
    }
  }
}
