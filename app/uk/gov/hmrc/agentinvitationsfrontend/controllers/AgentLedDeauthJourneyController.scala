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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import javax.inject.Inject
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.api.{Configuration, Logger}
import uk.gov.hmrc.agentinvitationsfrontend.config.{AppConfig, CountryNamesLoader, ExternalUrls}
import uk.gov.hmrc.agentinvitationsfrontend.forms.CommonConfirmationForms._
import uk.gov.hmrc.agentinvitationsfrontend.forms.{IrvClientForm, ItsaClientForm, VatClientForm, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.State.{IdentifyClientTrust, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyModel.Transitions._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.models.ClientType.{Business, Personal}
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, RelationshipsService}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.cancelAuthorisation.{ConfirmCancelPageConfig, SelectServicePageConfigCancel}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{ClientTypePageConfig, NotSignedUpPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.cancelAuthorisation.{authorisation_cancelled, business_select_service, business_select_single_service, client_type, confirm_cancel, confirm_client, no_client_found, response_failed, select_service, trust_select_service}
import uk.gov.hmrc.agentmtdidentifiers.model.Service
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.fsm.JourneyController

import scala.concurrent.ExecutionContext

class AgentLedDeauthJourneyController @Inject()(
  override val journeyService: AgentLedDeauthJourneyService,
  authActions: AuthActionsImpl,
  invitationsService: InvitationsService,
  relationshipsService: RelationshipsService,
  countryNamesLoader: CountryNamesLoader,
  notSignedUpPageConfig: NotSignedUpPageConfig,
  clientTypeView: client_type,
  businessSelectSingleServiceView: business_select_single_service,
  businessSelectServiceView: business_select_service,
  identifyClientItsaView: identify_client_itsa,
  identifyClientIrvView: identify_client_irv,
  identifyClientVatView: identify_client_vat,
  identifyClientTrustView: identify_client_trust,
  identifyClientCgtView: identify_client_cgt,
  identifyClientPptView: identify_client_ppt,
  confirmClientView: confirm_client,
  confirmCountryCodeCgtView: confirm_countryCode_cgt,
  confirmPostcodeCgtView: confirm_postcode_cgt,
  authCancelledView: authorisation_cancelled,
  notAuthorisedView: not_authorised,
  selectServiceView: select_service,
  noClientFoundView: no_client_found,
  trustSelectServiceView: trust_select_service,
  notSignedupView: not_signed_up,
  responseFailedView: response_failed,
  confirmCancelView: confirm_cancel,
  override val actionBuilder: DefaultActionBuilder
)(
  implicit ec: ExecutionContext,
  implicit val contactFrontendConfig: ContactFrontendConfig,
  configuration: Configuration,
  appConfig: AppConfig,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val cc: MessagesControllerComponents)
    extends FrontendController(cc) with JourneyController[HeaderCarrier] with I18nSupport {

  override def context(implicit rh: RequestHeader): HeaderCarrier = hc

  import authActions._
  import invitationsService._
  import relationshipsService._
  import uk.gov.hmrc.play.fsm.OptionalFormOps._

  private val countries = countryNamesLoader.load
  private val validCountryCodes = countries.keys.toSet
  private val urnEnabled = appConfig.featuresEnableTrustURNIdentifier

  val AsAgent: WithAuthorised[AuthorisedAgent] = { implicit request: Request[Any] =>
    withAuthorisedAsAgent(_)
  }

  val agentLedDeauthRoot: Action[AnyContent] = Action(Redirect(routes.AgentLedDeauthJourneyController.showClientType()))

  def showClientType: Action[AnyContent] =
    if (featureFlags.showAgentLedDeAuth) actions.whenAuthorised(AsAgent).show[SelectClientType.type]
    else {
      Logger(getClass).warn("Agent led de authorisation feature is disabled.")
      Action(NotImplemented)
    }

  def submitClientType: Action[AnyContent] =
    actions.whenAuthorisedWithRetrievals(AsAgent).bindForm(ClientTypeForm.deAuthorisationForm)(selectedClientType)

  def showSelectService: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[SelectService].orRollback

  def submitPersonalService: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .apply(
        chosenPersonalService(
          featureFlags.showHmrcMtdIt,
          featureFlags.showPersonalIncome,
          featureFlags.showHmrcMtdVat,
          featureFlags.showHmrcCgt,
          featureFlags.showPlasticPackagingTax))

  def submitBusinessServiceSingle: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.selectSingleServiceForm(Service.Vat, Business))(
        chosenBusinessService(featureFlags.showHmrcMtdVat, featureFlags.showPlasticPackagingTax))

  def submitBusinessService: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)
      .apply(
        chosenBusinessServiceMulti(
          featureFlags.showHmrcMtdVat,
          featureFlags.showPlasticPackagingTax
        )
      )

  def submitTrustService: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ServiceTypeForm.form)(chosenTrustService(featureFlags.showHmrcTrust, featureFlags.showHmrcCgt, featureFlags.showPlasticPackagingTax))

  val identifyClientRedirect: Action[AnyContent] = Action(Redirect(routes.AgentLedDeauthJourneyController.showIdentifyClient()))

  def showIdentifyClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[IdentifyClient].orRollback

  def submitIdentifyItsaClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(ItsaClientForm.form)
      .applyWithRequest(implicit request => submitIdentifyClientItsa(checkPostcodeMatches, getClientNameByService, hasActiveRelationshipFor))

  def submitIdentifyIrvClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(IrvClientForm.form)
      .applyWithRequest(implicit request =>
        submitIdentifyClientIrv(checkCitizenRecordMatches, getClientNameByService, hasActiveRelationshipFor, hasPartialAuthorisationFor))

  def submitIdentifyVatClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(VatClientForm.form)
      .applyWithRequest(implicit request =>
        submitIdentifyClientVat(checkVatRegistrationDateMatches, getClientNameByService, hasActiveRelationshipFor))

  val submitIdentifyTrustClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(TrustClientForm.form(urnEnabled))
    .applyWithRequest(implicit request => submitIdentifyClientTrust(taxId => acaConnector.getTrustName(taxId.value)))

  val submitIdentifyCgtClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(CgtClientForm.form())
    .applyWithRequest(implicit request => submitIdentifyClientCgt(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  val submitIdentifyPptClient: Action[AnyContent] = actions
    .whenAuthorisedWithRetrievals(AsAgent)
    .bindForm(PptClientForm.form)
    .applyWithRequest(implicit request => submitIdentifyClientPpt(pptRef => acaConnector.getPptSubscription(pptRef)))

  def showPostcodeCgt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmPostcodeCgt].orRollback

  def submitConfirmCgtPostcode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(PostcodeForm.form)
      .applyWithRequest(implicit request => confirmPostcodeCgt(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  def showCountryCodeCgt: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCountryCodeCgt].orRollback

  def submitConfirmCgtCountryCode: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(CountrycodeForm.form(validCountryCodes))
      .applyWithRequest(implicit request => confirmCountryCodeCgt(cgtRef => acaConnector.getCgtSubscription(cgtRef)))

  def showConfirmClient: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmClient].orRollback

  def submitConfirmClient: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(confirmCancelForm)
      .applyWithRequest(implicit request => clientConfirmed(hasActiveRelationshipFor)(hasPartialAuthorisationFor))

  def showConfirmCancel: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ConfirmCancel].orRollback

  def submitConfirmCancel: Action[AnyContent] =
    actions
      .whenAuthorisedWithRetrievals(AsAgent)
      .bindForm(confirmCancelForm)
      .applyWithRequest(implicit request => cancelConfirmed(deleteRelationshipForService, getAgencyName, setRelationshipEnded))

  def showAuthorisationCancelled: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[AuthorisationCancelled]

  def showKnownFactNotMatched: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ErrorState]

  def showNotSignedUp: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotSignedUp]

  def showNotAuthorised: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[NotAuthorised]

  def showResponseFailed: Action[AnyContent] = actions.whenAuthorised(AsAgent).show[ResponseFailed]

  override def getCallFor(state: journeyService.model.State)(implicit request: Request[_]): Call = state match {
    case SelectClientType            => routes.AgentLedDeauthJourneyController.showClientType()
    case _: SelectService            => routes.AgentLedDeauthJourneyController.showSelectService()
    case _: IdentifyClientPersonal   => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case _: IdentifyClientBusiness   => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case IdentifyClientTrust         => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case IdentifyClientCgt           => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case IdentifyClientPpt           => routes.AgentLedDeauthJourneyController.showIdentifyClient()
    case _: ConfirmPostcodeCgt       => routes.AgentLedDeauthJourneyController.showPostcodeCgt()
    case _: ConfirmCountryCodeCgt    => routes.AgentLedDeauthJourneyController.showCountryCodeCgt()
    case _: ConfirmClientItsa        => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientIrv         => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientPersonalVat => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientBusiness    => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientTrust       => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientTrustNT     => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientCgt         => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmClientPpt         => routes.AgentLedDeauthJourneyController.showConfirmClient()
    case _: ConfirmCancel            => routes.AgentLedDeauthJourneyController.showConfirmCancel()
    case _: AuthorisationCancelled   => routes.AgentLedDeauthJourneyController.showAuthorisationCancelled()
    case KnownFactNotMatched         => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched()
    case TrustNotFound               => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched()
    case _: CgtRefNotFound           => routes.AgentLedDeauthJourneyController.showKnownFactNotMatched()
    case _: NotSignedUp              => routes.AgentLedDeauthJourneyController.showNotSignedUp()
    case _: NotAuthorised            => routes.AgentLedDeauthJourneyController.showNotAuthorised()
    case _: ResponseFailed           => routes.AgentLedDeauthJourneyController.showResponseFailed()
    case _                           => throw new Exception(s"Link not found for $state")
  }

  override def renderState(state: journeyService.model.State, breadcrumbs: journeyService.Breadcrumbs, formWithErrors: Option[Form[_]])(
    implicit request: Request[_]): Result = state match {

    case SelectClientType =>
      def backLinkForClientType(implicit request: Request[_]): String =
        breadcrumbs.headOption.fold(externalUrls.agentServicesAccountUrl)(getCallFor(_).url)

      Ok(
        clientTypeView(
          formWithErrors.or(ClientTypeForm.deAuthorisationForm),
          ClientTypePageConfig(backLinkForClientType, routes.AgentLedDeauthJourneyController.submitClientType(), featureFlags.showHmrcTrust)
        ))

    case SelectService(ClientType.Personal, enabledServices) =>
      Ok(
        selectServiceView(
          formWithErrors.or(ServiceTypeForm.form),
          SelectServicePageConfigCancel(
            ClientType.Personal,
            featureFlags,
            enabledServices,
            routes.AgentLedDeauthJourneyController.submitPersonalService(),
            backLinkFor(breadcrumbs).url
          )
        ))

    case SelectService(ClientType.Business, enabledServices) =>
      val pageConfig = SelectServicePageConfigCancel(
        ClientType.Business,
        featureFlags,
        enabledServices,
        routes.AgentLedDeauthJourneyController.submitBusinessService(),
        backLinkFor(breadcrumbs).url
      )
      pageConfig.enabledServices.size match {
        case 1 =>
          Ok(
            businessSelectSingleServiceView(
              formWithErrors.or(ServiceTypeForm.selectSingleServiceForm(enabledServices.head, Business)),
              routes.AgentLedDeauthJourneyController.submitBusinessServiceSingle(),
              backLinkFor(breadcrumbs).url
            )
          )
        case x if x > 1 =>
          Ok(
            businessSelectServiceView(
              formWithErrors.or(ServiceTypeForm.form),
              pageConfig
            )
          )
      }

    case SelectService(ClientType.Trust, enabledServices) =>
      Ok(
        trustSelectServiceView(
          formWithErrors.or(ServiceTypeForm.form),
          SelectServicePageConfigCancel(
            ClientType.Trust,
            featureFlags,
            enabledServices,
            routes.AgentLedDeauthJourneyController.submitTrustService(),
            backLinkFor(breadcrumbs).url
          )
        )
      )

    case IdentifyClientPersonal(service) =>
      service match {
        case Service.MtdIt =>
          Ok(
            identifyClientItsaView(
              formWithErrors.or(ItsaClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyItsaClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            ))
        case Service.PersonalIncomeRecord =>
          Ok(
            identifyClientIrvView(
              formWithErrors.or(IrvClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyIrvClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            )
          )
        case Service.Vat =>
          Ok(
            identifyClientVatView(
              formWithErrors.or(VatClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            ))
        case Service.CapitalGains =>
          Ok(
            identifyClientCgtView(
              formWithErrors.or(CgtClientForm.form()),
              routes.AgentLedDeauthJourneyController.submitIdentifyCgtClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            ))
        case Service.Ppt =>
          Ok(
            identifyClientPptView(
              formWithErrors.or(PptClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyPptClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            ))
      }

    case IdentifyClientBusiness(service) =>
      service match {
        case Service.Vat =>
          Ok(
            identifyClientVatView(
              formWithErrors.or(VatClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyVatClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            ))
        case Service.Ppt =>
          Ok(
            identifyClientPptView(
              formWithErrors.or(PptClientForm.form),
              routes.AgentLedDeauthJourneyController.submitIdentifyPptClient(),
              backLinkFor(breadcrumbs).url,
              isDeAuthJourney = true
            ))
      }

    case IdentifyClientTrust =>
      Ok(
        identifyClientTrustView(
          formWithErrors.or(TrustClientForm.form(urnEnabled)),
          routes.AgentLedDeauthJourneyController.submitIdentifyTrustClient(),
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true,
          showUrnEnabledContent = urnEnabled
        ))

    case IdentifyClientCgt =>
      Ok(
        identifyClientCgtView(
          formWithErrors.or(CgtClientForm.form()),
          routes.AgentLedDeauthJourneyController.submitIdentifyCgtClient(),
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        ))

    case IdentifyClientPpt =>
      Ok(
        identifyClientPptView(
          formWithErrors.or(PptClientForm.form),
          routes.AgentLedDeauthJourneyController.submitIdentifyPptClient(),
          backLinkFor(breadcrumbs).url,
          isDeAuthJourney = true
        ))

    case ConfirmClientItsa(clientName, nino) =>
      Ok(
        confirmClientView(
          clientName.getOrElse(""),
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "nino",
          nino.value
        ))

    case ConfirmClientIrv(clientName, nino) =>
      Ok(
        confirmClientView(
          clientName.getOrElse(""),
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "nino",
          nino.value
        ))

    case ConfirmClientPersonalVat(clientName, vrn) =>
      Ok(
        confirmClientView(
          clientName.getOrElse(""),
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "vrn",
          vrn.value
        ))

    case ConfirmClientBusiness(clientName, vrn) =>
      Ok(
        confirmClientView(
          clientName.getOrElse(""),
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "vrn",
          vrn.value
        ))

    case ConfirmClientTrust(trustName, utr) =>
      Ok(
        confirmClientView(
          trustName,
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "utr",
          utr.value
        ))

    case ConfirmClientTrustNT(trustName, urn) =>
      Ok(
        confirmClientView(
          trustName,
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "urn",
          urn.value
        ))

    case _: ConfirmCountryCodeCgt =>
      Ok(
        confirmCountryCodeCgtView(
          Personal,
          countries,
          formWithErrors.or(CountrycodeForm.form(validCountryCodes)),
          backLinkFor(breadcrumbs).url,
          fromFastTrack = false,
          isDeAuth = true))

    case _: ConfirmPostcodeCgt =>
      Ok(confirmPostcodeCgtView(Personal, formWithErrors.or(PostcodeForm.form), backLinkFor(breadcrumbs).url, fromFastTrack = false, isDeAuth = true))

    case ConfirmClientCgt(cgtRef, clientName) =>
      Ok(
        confirmClientView(
          clientName,
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "CGTPDRef",
          cgtRef.value
        ))

    case ConfirmClientPpt(pptRef, clientName) =>
      Ok(
        confirmClientView(
          clientName,
          formWithErrors.or(confirmCancelForm),
          routes.AgentLedDeauthJourneyController.submitConfirmClient(),
          backLinkFor(breadcrumbs).url,
          "EtmpRegistrationNumber",
          pptRef.value
        ))

    case ConfirmCancel(service, clientName, _, _) =>
      Ok(
        confirmCancelView(
          formWithErrors.or(confirmCancelForm),
          ConfirmCancelPageConfig(
            service,
            clientName.getOrElse(""),
            routes.AgentLedDeauthJourneyController.submitConfirmCancel(),
            backLinkFor(breadcrumbs).url)
        ))

    case AuthorisationCancelled(service, clientName, agencyName) =>
      journeyService.cleanBreadcrumbs(_ => Nil)
      Ok(authCancelledView(service, clientName.getOrElse(""), agencyName, externalUrls.agentServicesAccountUrl))

    case KnownFactNotMatched =>
      Ok(noClientFoundView(routes.AgentLedDeauthJourneyController.showClientType()))

    case TrustNotFound =>
      Ok(noClientFoundView(routes.AgentLedDeauthJourneyController.showClientType()))

    case CgtRefNotFound(_) =>
      Ok(noClientFoundView(routes.AgentLedDeauthJourneyController.showClientType()))

    case NotSignedUp(service) =>
      Ok(notSignedupView(service, hasRequests = false, isDeAuthJourney = true, notSignedUpPageConfig.render(service)))

    case NotAuthorised(service) =>
      Ok(notAuthorisedView(service, routes.AgentLedDeauthJourneyController.showClientType(), isDeAuthJourney = true))

    case _: ResponseFailed => Ok(responseFailedView(routes.AgentLedDeauthJourneyController.submitConfirmCancel()))

  }
}
