/*
 * Copyright 2018 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.agentinvitationsfrontend.views.clients.{MultiConfirmDeclinePageConfig, MultiConfirmTermsPageConfig, MultiInvitationDeclinedPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

case class ConfirmedTerms(itsaConsent: Boolean, afiConsent: Boolean, vatConsent: Boolean)

@Singleton
class ClientsMultiInvitationController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  val messagesApi: play.api.i18n.MessagesApi,
  multiInvitationCache: MultiInvitationsCache,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls)
    extends FrontendController with I18nSupport with AuthActions {

  import ClientsInvitationController._
  import ClientsMultiInvitationController._

  def warmUp(clientType: String, uid: String, normalisedAgentName: String): Action[AnyContent] = Action.async {
    implicit request =>
      for {
        record <- invitationsConnector.getAgentReferenceRecord(uid)
        result <- record match {
                   case Some(r) if r.normalisedAgentNames.contains(normalisedAgentName) => {
                     invitationsService.getAgencyName(r.arn).map { name =>
                       Ok(warm_up(name, clientType, uid))
                     }
                   }
                   case _ => Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
                 }

      } yield result
  }

  def getMultiConfirmTerms(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      withAgencyNameAndConsents(uid, Pending) { (agencyName, consents) =>
        multiInvitationCache
          .save(MultiInvitationsCacheItem(consents, Some(agencyName)))
          .map { _ =>
            Ok(
              confirm_terms_multi(
                confirmTermsMultiForm,
                MultiConfirmTermsPageConfig(agencyName, clientType, uid, consents)))
          }
      }.recover {
        case _: NotFoundException =>
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
      }
    }
  }

  def updateMultiInvitation(confirmedTerms: ConfirmedTerms)(
    item: MultiInvitationsCacheItem): MultiInvitationsCacheItem = {

    val hasConsent: String => Boolean = {
      case "itsa" => confirmedTerms.itsaConsent
      case "afi"  => confirmedTerms.afiConsent
      case "vat"  => confirmedTerms.vatConsent
    }

    item.copy(consents = item.consents.map(c => c.copy(consent = hasConsent(c.serviceKey))))
  }

  def submitMultiConfirmTerms(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      confirmTermsMultiForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            for {
              cacheItemOpt <- multiInvitationCache.fetch()
              result <- cacheItemOpt match {
                         case None => Future.failed(new BadRequestException("Invalid journey state."))
                         case Some(cacheItem) =>
                           Future successful Ok(
                             confirm_terms_multi(
                               formWithErrors,
                               MultiConfirmTermsPageConfig(
                                 cacheItem.agencyName.getOrElse(throw new Exception("Lost agency name")),
                                 clientType,
                                 uid,
                                 cacheItem.consents)
                             ))
                       }
            } yield result,
          confirmedTerms => {
            for {
              consents <- multiInvitationCache.updateWith(updateMultiInvitation(confirmedTerms))
            } yield {
              Redirect(routes.ClientsMultiInvitationController.showCheckAnswers())
            }
          }
        )
    }
  }

  def showCheckAnswers() = Action.async { implicit request =>
    Future.successful(Ok) //TODO check_answers page exists
  }

  def getMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      withAgencyNameAndConsents(uid, Pending) { (agencyName, consents) =>
        multiInvitationCache
          .save(MultiInvitationsCacheItem(consents, Some(agencyName)))
          .map { _ =>
            Ok(
              confirm_decline(
                confirmDeclineForm,
                MultiConfirmDeclinePageConfig(agencyName, clientType, uid, consents.map(_.serviceKey))))
          }
      }.recover {
        case _: NotFoundException =>
          Redirect(routes.ClientsInvitationController.notFoundInvitation())
      }
    }
  }

  def submitMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async {
    implicit request =>
      withAuthorisedAsAnyClient { _ =>
        for {
          cacheItemOpt <- multiInvitationCache.fetch()
          result <- cacheItemOpt match {
                     case None => Future.failed(new BadRequestException("Invalid journey state."))
                     case Some(cachedItem) =>
                       confirmDeclineForm
                         .bindFromRequest()
                         .fold(
                           formWithErrors =>
                             Future successful Ok(confirm_decline(
                               formWithErrors,
                               MultiConfirmDeclinePageConfig(
                                 cachedItem.agencyName.getOrElse(throw new Exception("Lost agency name")),
                                 clientType,
                                 uid,
                                 cachedItem.consents.map(_.serviceKey))
                             )),
                           _ =>
                             for {
                               recordOpt <- invitationsConnector.getAgentReferenceRecord(uid)
                               _ <- recordOpt match {
                                     case None => Future.failed(new BadRequestException("Invalid journey state."))
                                     case Some(record) =>
                                       Future.sequence(cachedItem.consents.map(c =>
                                         invitationsService.rejectInvitation(record.arn, c.invitationId)))
                                   }
                             } yield Redirect(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid))
                         )
                   }
        } yield result
      }
  }

  def getMultiInvitationsDeclined(uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      for {
        invitationIds <- invitationsConnector.getAllClientInvitationIdsByStatus(uid, Rejected)
        agencyName    <- getAgencyName(uid)
        serviceKeys = invitationIds.map(id => Services.determineServiceMessageKey(id))

      } yield
        if (serviceKeys.nonEmpty)
          Ok(invitation_declined(MultiInvitationDeclinedPageConfig(agencyName, serviceKeys)))
        else Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

  private def getAgencyName(uid: String)(implicit hc: HeaderCarrier): Future[String] =
    for {
      record <- invitationsConnector.getAgentReferenceRecord(uid)
      name <- record match {
               case Some(rec) => invitationsService.getAgencyName(rec.arn)
               case None      => Future.failed(new Exception(s"Agent Reference Record not found for $uid"))
             }
    } yield name

  private def withAgencyNameAndConsents(uid: String, status: InvitationStatus)(
    body: ((String, Seq[Consent]) => Future[Result]))(implicit hc: HeaderCarrier): Future[Result] =
    for {
      invitationIds <- invitationsConnector.getAllClientInvitationIdsByStatus(uid, status)
      agencyName    <- getAgencyName(uid)
      consents = invitationIds.map(id => Consent(id, Services.determineServiceMessageKey(id), consent = false))
      result <- body(agencyName, consents)
    } yield result

}

object ClientsMultiInvitationController {

  val confirmTermsMultiForm: Form[ConfirmedTerms] =
    Form[ConfirmedTerms](
      mapping(
        "confirmedTerms.itsa" -> checked("Not Chosen ITSA"),
        "confirmedTerms.afi"  -> checked("Not Chosen AFI"),
        "confirmedTerms.vat"  -> checked("Not Chosen VAT")
      )(ConfirmedTerms.apply)(ConfirmedTerms.unapply))
}
