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
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Result}
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.models._
import uk.gov.hmrc.agentinvitationsfrontend.services.{InvitationsService, MultiInvitationCache, MultiInvitationsCacheInput, SelectedInvitation}
import uk.gov.hmrc.agentinvitationsfrontend.views.clients.{MultiConfirmDeclinePageConfig, MultiConfirmTermsPageConfig, MultiInvitationDeclinedPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.agentmtdidentifiers.model.{InvitationId, MtdItId, Vrn}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

case class ConfirmTermsMultiForm(invitationIds: Seq[String], confirmTerms: Seq[Boolean])

@Singleton
class ClientsMultiInvitationController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  val messagesApi: play.api.i18n.MessagesApi,
  multiInvitationCache: MultiInvitationCache,
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
      getPageComponents(uid, Pending) { (invitationIds, agencyName, serviceKeys) =>
        multiInvitationCache.save(
          MultiInvitationsCacheInput(
            invitationIds
              .zip(Seq.fill(invitationIds.length)(false))
              .map(choices => SelectedInvitation(choices._1, choices._2))))

        Ok(
          confirm_terms_multi(
            confirmTermsMultiForm,
            MultiConfirmTermsPageConfig(agencyName, clientType, uid, serviceKeys.map(_._2))))
      //At this point store sequence of invitations with a boolean for yes-i want to authorise or no (checkboxes) automatically set to NO
      //Store: invitationId?, serviceKey, bool

        //Note I think InvitationId could be paired with service keys and use it as hidden input???
      }
    }
  }

  def processChoices(givenInvitationIds: Seq[InvitationId], selectedInvitations: Seq[String], choices: Seq[Boolean]) = {
    val selected =
      selectedInvitations.zip(choices).map(selected => SelectedInvitation(InvitationId(selected._1), selected._2))
    val givenNotSelected = givenInvitationIds
      .filterNot { chosen =>
        selectedInvitations.contains(chosen.value)
      }
      .map(notChosen => SelectedInvitation(notChosen, false))
    givenNotSelected ++ selected
  }

  def submitMultiConfirmTerms(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      confirmTermsMultiForm
        .bindFromRequest()
        .fold(
          formWithErrors => Future successful Redirect(routes.ClientsInvitationController.notAuthorised()),
          data => {

            for {
              invitationIds <- invitationsConnector.getAllClientInvitationIdsByStatus(uid, Pending)
              _ <- {
                val processedInvitations = processChoices(invitationIds, data.invitationIds, data.confirmTerms)
                multiInvitationCache.save(MultiInvitationsCacheInput(processedInvitations))
              }
            } yield {
              println(s"WHIT WHOO OWL: ${data.confirmTerms}")
              Redirect(routes.ClientsInvitationController.notFoundInvitation())
            }

//            multiInvitationCache
//              .updateIsSelected(data.invitationIds.zip(data.confirmTerms).map(choices => SelectedInvitation(InvitationId(choices._1), choices._2)))
//              .map(_ => {
//                println(s"WHIT WHOO OWL: ${data.confirmTerms}")
//                Redirect(routes.ClientsInvitationController.notFoundInvitation())
//              })
          }
        )
    }
  }

  def getMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      for {
        agentReferenceRecordOpt <- invitationsConnector.getAgentReferenceRecord(uid)
        result <- agentReferenceRecordOpt match {
                   case Some(agentReferenceRecord) =>
                     for {
                       invitationIds <- invitationsConnector.getAllClientInvitationIdsByStatus(uid, Pending)
                       agencyName    <- invitationsService.getAgencyName(agentReferenceRecord.arn)
                       serviceKeys = invitationIds.map(id => Services.determineServiceMessageKey(id))
                     } yield
                       if (serviceKeys.nonEmpty) {
                         Ok(
                           confirm_decline(
                             confirmDeclineForm,
                             MultiConfirmDeclinePageConfig(agencyName, clientType, uid, serviceKeys)))
                       } else
                         Redirect(routes.ClientsInvitationController.notFoundInvitation())

                   case None =>
                     Future.failed(new Exception(s"Agent Reference Record not found for $uid"))
                 }
      } yield result
    }
  }

  private def getAgencyNameViaClient(uid: String)(implicit hc: HeaderCarrier): Future[String] =
    for {
      record <- invitationsConnector.getAgentReferenceRecord(uid)
      name <- record match {
               case Some(rec) => invitationsService.getAgencyName(rec.arn)
               case None      => Future.failed(new Exception(s"Agent Reference Record not found for $uid"))
             }
    } yield name

  private def rejectPendingInvitation(clientId: String, invitationId: InvitationId, apiIdentifier: String)(
    implicit hc: HeaderCarrier) =
    for {
      isPending <- invitationsService
                    .getClientInvitation(clientId, invitationId, apiIdentifier)
                    .map(inv => inv.status == "Pending")
      _ <- if (isPending) {
            apiIdentifier match {
              case MTDITID =>
                invitationsService.rejectITSAInvitation(invitationId, MtdItId(clientId))
              case NI =>
                invitationsService.rejectAFIInvitation(invitationId, Nino(clientId))
              case VRN =>
                invitationsService.rejectVATInvitation(invitationId, Vrn(clientId))
            }
          } else Future successful (())
    } yield ()

  private def getPageComponents(uid: String, status: InvitationStatus)(
    body: (Seq[InvitationId], String, Seq[(String, String)]) => Result)(implicit hc: HeaderCarrier) =
    for {
      invitationIds <- invitationsConnector.getAllClientInvitationIdsByStatus(uid, status)
      agencyName    <- getAgencyNameViaClient(uid)
      serviceKeys = invitationIds.map(id => (id.value, Services.determineServiceMessageKey(id)))
    } yield body(invitationIds, agencyName, serviceKeys)

  private def assignInvitations(
    invitationIds: Seq[InvitationId],
    identifiers: Seq[(String, String)]): Seq[ClientInfo] = {
    def collate(
      invitationIds: Seq[InvitationId],
      identifiers: Seq[(String, String)],
      clientInfoCollection: Seq[ClientInfo]): Seq[ClientInfo] =
      invitationIds match {
        case Nil => clientInfoCollection
        case head :: remainingInvitationIds =>
          determineService(head) match {
            case ValidService(HMRCMTDIT, _, MTDITID, MTDITID, _) =>
              identifiers.find(_._1 == MTDITID) match {
                case None => collate(remainingInvitationIds, identifiers, clientInfoCollection)
                case Some((identifier, clientId)) =>
                  collate(
                    remainingInvitationIds,
                    identifiers,
                    clientInfoCollection ++ Seq(ClientInfo(head, identifier, clientId, MTDITID)))
              }
            case ValidService(HMRCPIR, _, NINO, NI, _) =>
              identifiers.find(_._1 == NINO) match {
                case None => collate(remainingInvitationIds, identifiers, clientInfoCollection)
                case Some((identifier, clientId)) =>
                  collate(
                    remainingInvitationIds,
                    identifiers,
                    clientInfoCollection ++ Seq(ClientInfo(head, identifier, clientId, NI)))
              }
            case ValidService(HMRCMTDVAT, _, VRN, VRN, _) =>
              identifiers.find(_._1 == VRN) match {
                case None => collate(remainingInvitationIds, identifiers, clientInfoCollection)
                case Some((identifier, clientId)) =>
                  collate(
                    remainingInvitationIds,
                    identifiers,
                    clientInfoCollection ++ Seq(ClientInfo(head, identifier, clientId, VRN)))
              }
            case InvalidService => {
              Logger(getClass).warn("Something has gone wrong here: Invalid Service")
              collate(remainingInvitationIds, identifiers, clientInfoCollection)
            }
            case _ => {
              Logger(getClass).warn("Something has gone wrong here: Did not meet anything")
              collate(remainingInvitationIds, identifiers, clientInfoCollection)
            }

          }
      }
    collate(invitationIds, identifiers, Seq.empty)
  }

  def submitMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async {
    implicit request =>
      withAuthorisedAsAnyClient { identifiers =>
        confirmDeclineForm
          .bindFromRequest()
          .fold(
            formWithErrors => {

              getPageComponents(uid, Pending) { (_, agencyName, serviceKeys) =>
                Ok(
                  confirm_decline(
                    formWithErrors,
                    MultiConfirmDeclinePageConfig(agencyName, clientType, uid, serviceKeys.map(_._2))))
              }
            },
            data => {
              if (data.value.getOrElse(false)) {
                getPageComponents(uid, Pending) { (invitationIds, _, _) =>
                  Future.traverse(assignInvitations(invitationIds, identifiers)) {
                    case ClientInfo(invitationId, _, clientId, apiIdentifier) =>
                      rejectPendingInvitation(clientId, invitationId, apiIdentifier)
                  }
                  Redirect(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid))

                }
              } else {
                //Confirm Terms Multi Page Not There Yet
                Future.successful(NotImplemented)
              }
            }
          )
      }
  }

  def getMultiInvitationsDeclined(uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { _ =>
      for {
        invitationIds <- invitationsConnector.getAllClientInvitationIdsByStatus(uid, Rejected)
        agencyName    <- getAgencyNameViaClient(uid)
        serviceKeys = invitationIds.map(id => Services.determineServiceMessageKey(id))
      } yield
        if (serviceKeys.nonEmpty)
          Ok(invitation_declined(MultiInvitationDeclinedPageConfig(agencyName, serviceKeys)))
        else Redirect(routes.ClientsInvitationController.notFoundInvitation())
    }
  }

}

object ClientsMultiInvitationController {

  val confirmTermsMultiForm: Form[ConfirmTermsMultiForm] =
    Form[ConfirmTermsMultiForm](
      mapping("invitationIds" -> seq(text), "confirmTerms" -> seq(boolean))((invitationIds, confirmTerms) =>
        ConfirmTermsMultiForm(invitationIds, confirmTerms))(answer => Some(answer.invitationIds, answer.confirmTerms)))
}
