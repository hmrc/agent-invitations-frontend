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
import uk.gov.hmrc.agentinvitationsfrontend.views.clients._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.clients._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent

@Singleton
class ClientsMultiInvitationController @Inject()(
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  val messagesApi: play.api.i18n.MessagesApi,
  journeyStateCache: ClientConsentsJourneyStateCache,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls)
    extends FrontendController with I18nSupport with AuthActions {

  import ClientsInvitationController.confirmDeclineForm
  import ClientsMultiInvitationController._

  object targets {
    val NotFoundInvitation = Future successful Redirect(routes.ClientsInvitationController.notFoundInvitation())
    val InvalidJourneyState = Future.failed(new BadRequestException("Invalid journey state."))
  }

  private def matchClientTypeToGroup(affinityGroup: String, clientType: String): Boolean =
    (affinityGroup, clientType) match {
      case ("Individual", "personal")   => true
      case ("Organisation", "business") => true
      case _                            => false
    }

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
                   case _ => targets.NotFoundInvitation
                 }

      } yield result
  }

  def getMultiConfirmTerms(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (affinity, _) =>
      if (matchClientTypeToGroup(affinity, clientType)) {
        withAgencyNameAndConsents(uid, Pending) { (agencyName, consents) =>
          if (consents.nonEmpty) {
            journeyStateCache
              .save(ClientConsentsJourneyState(consents, Some(agencyName)))
              .map { _ =>
                Ok(
                  confirm_terms_multi(
                    confirmTermsMultiForm,
                    MultiConfirmTermsPageConfig(agencyName, clientType, uid, consents)))
              }
          } else targets.NotFoundInvitation
        }.recoverWith {
          case _: NotFoundException => targets.NotFoundInvitation
        }
      } else {
        Future successful Redirect(routes.ClientErrorController.incorrectClientType())
          .addingToSession("clientType" -> clientType)
      }
    }
  }

  def getMultiConfirmTermsIndividual(clientType: String, uid: String, givenServiceKey: String): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAnyClient { (_, _) =>
        for {
          journeyState <- journeyStateCache.get
          result <- {
            val chosenConsent = journeyState.consents.find(_.serviceKey == givenServiceKey).toSeq
            if (chosenConsent.nonEmpty) {
              Future successful Ok(
                confirm_terms_multi(
                  confirmTermsMultiForm,
                  MultiConfirmTermsPageConfig(
                    journeyState.agencyName.getOrElse(throw new Exception("Lost agency name")),
                    clientType,
                    uid,
                    chosenConsent)
                )).addingToSession("whichConsent" -> givenServiceKey)
            } else {
              targets.InvalidJourneyState
            }
          }
        } yield result
      }
    }

  def submitMultiConfirmTerms(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (affinity, _) =>
      if (matchClientTypeToGroup(affinity, clientType)) {
        val itsaChoice = request.session.get("itsaChoice").getOrElse("false").toBoolean
        val afiChoice = request.session.get("afiChoice").getOrElse("false").toBoolean
        val vatChoice = request.session.get("vatChoice").getOrElse("false").toBoolean
        val serviceKey = request.session.get("whichConsent").getOrElse("")
        val confirmedTermsSession = ConfirmedTerms(itsaChoice, afiChoice, vatChoice)
        confirmTermsMultiForm
          .bindFromRequest()
          .value match {
          case Some(confirmedTerms) => {
            val updatedConfirmedTerms = determineNewTerms(serviceKey, confirmedTermsSession, confirmedTerms)
            for {
              _ <- journeyStateCache.transform(updateMultiInvitation(updatedConfirmedTerms))
            } yield {
              Redirect(routes.ClientsMultiInvitationController.showCheckAnswers(clientType, uid))
                .addingToSession(
                  "itsaChoice" -> updatedConfirmedTerms.itsaConsent.toString,
                  "afiChoice"  -> updatedConfirmedTerms.afiConsent.toString,
                  "vatChoice"  -> updatedConfirmedTerms.vatConsent.toString
                )
            }
          }
          case None => targets.InvalidJourneyState
        }
      } else {
        Future successful Redirect(routes.ClientErrorController.incorrectClientType())
          .addingToSession("clientType" -> clientType)
      }
    }
  }

  def determineNewTerms(
    individualServiceKey: String,
    sessionConfirmTerms: ConfirmedTerms,
    boundConfirmTerms: ConfirmedTerms): ConfirmedTerms =
    individualServiceKey match {
      case "itsa" =>
        ConfirmedTerms(boundConfirmTerms.itsaConsent, sessionConfirmTerms.afiConsent, sessionConfirmTerms.vatConsent)
      case "afi" =>
        ConfirmedTerms(sessionConfirmTerms.itsaConsent, boundConfirmTerms.afiConsent, sessionConfirmTerms.vatConsent)
      case "vat" =>
        ConfirmedTerms(sessionConfirmTerms.itsaConsent, sessionConfirmTerms.afiConsent, boundConfirmTerms.vatConsent)
      case _ =>
        ConfirmedTerms(boundConfirmTerms.itsaConsent, boundConfirmTerms.afiConsent, boundConfirmTerms.vatConsent)
    }

  def showCheckAnswers(clientType: String, uid: String): Action[AnyContent] = Action.async { implicit request =>
    for {
      journeyState <- journeyStateCache.get
      result <- {
        Future.successful(
          Ok(
            check_answers(
              CheckAnswersPageConfig(
                journeyState.consents.map(c => c.serviceKey -> c).toMap.values.toSeq,
                journeyState.agencyName.getOrElse(throw new Exception("Lost agency name")),
                clientType,
                uid
              )
            )
          ))
      }
    } yield result
  }

  private def processConsents(consents: Seq[ClientConsent])(implicit hc: HeaderCarrier): Future[Seq[ClientConsent]] =
    for {
      result <- Future.traverse(consents) {
                 case chosenConsent @ ClientConsent(invitationId, _, _, consent, _) =>
                   if (consent) {
                     invitationsService
                       .acceptInvitation(invitationId)
                       .map(acceptSuccess => chosenConsent.copy(processed = acceptSuccess))
                   } else {
                     invitationsService
                       .rejectInvitation(invitationId)
                       .map(_ => chosenConsent.copy(processed = true))
                   }
               }
    } yield result

  def submitAnswers(uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (_, _) =>
      for {
        journeyState <- journeyStateCache.get
        result <- {
          processConsents(journeyState.consents).flatMap { updatedConsents =>
            {
              journeyStateCache.save(journeyState.copy(consents = updatedConsents)).map { modifiedCache =>
                if (modifiedCache.allDeclinedProcessed) {
                  Redirect(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid))
                } else if (modifiedCache.allAcceptanceFailed) {
                  Redirect(routes.ClientsMultiInvitationController.showAllResponsesFailed())
                } else if (modifiedCache.someAcceptanceFailed) {
                  Redirect(routes.ClientsMultiInvitationController.showSomeResponsesFailed())
                } else {
                  Redirect(routes.ClientsMultiInvitationController.invitationAccepted())
                }
              }
            }
          }
        }
      } yield result
    }
  }

  val showAllResponsesFailed: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (_, _) =>
      Future successful Ok(all_responses_failed())
    }
  }

  def showSomeResponsesFailed: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (_, _) =>
      journeyStateCache.get.flatMap { journeyState =>
        if (journeyState.consents.exists(_.processed == false) && journeyState.consents.exists(_.processed == true)) {
          Future successful Ok(
            some_responses_failed(SomeResponsesFailedPageConfig(
              journeyState.consents.filter(_.processed == false).map(c => c.serviceKey -> c).toMap.values.toSeq,
              journeyState.agencyName.getOrElse(throw new Exception("Lost agency name"))
            )))
        } else targets.InvalidJourneyState
      }
    }
  }

  val invitationAccepted: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (_, _) =>
      for {
        journeyState <- journeyStateCache.get
        result <- {
          Future successful Ok(
            complete(MultiCompletePageConfig(
              journeyState.agencyName.getOrElse(throw new Exception("Lost agency name")),
              journeyState.consents
                .filter(_.processed == true)
                .map(consent => consent.serviceKey -> consent)
                .toMap
                .values
                .toSeq
            )))
        }
      } yield result
    }
  }

  def getMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] =
    Action.async { implicit request =>
      withAuthorisedAsAnyClient { (affinity, _) =>
        if (matchClientTypeToGroup(affinity, clientType)) {
          withAgencyNameAndConsents(uid, Pending) { (agencyName, consents) =>
            journeyStateCache
              .save(ClientConsentsJourneyState(consents, Some(agencyName)))
              .map { _ =>
                Ok(
                  confirm_decline(
                    confirmDeclineForm,
                    MultiConfirmDeclinePageConfig(agencyName, clientType, uid, consents.map(_.serviceKey).distinct)))
              }
          }.recoverWith {
            case _: NotFoundException => targets.NotFoundInvitation
          }
        } else {
          Future successful Redirect(routes.ClientErrorController.incorrectClientType())
            .addingToSession("clientType" -> clientType)
        }
      }
    }

  def submitMultiConfirmDecline(clientType: String, uid: String): Action[AnyContent] = Action.async {
    implicit request =>
      withAuthorisedAsAnyClient { (affinity, _) =>
        if (matchClientTypeToGroup(affinity, clientType)) {
          for {
            journeyState <- journeyStateCache.get
            result <- {
              confirmDeclineForm
                .bindFromRequest()
                .fold(
                  formWithErrors =>
                    Future successful Ok(confirm_decline(
                      formWithErrors,
                      MultiConfirmDeclinePageConfig(
                        journeyState.agencyName.getOrElse(throw new Exception("Lost agency name")),
                        clientType,
                        uid,
                        journeyState.consents.map(_.serviceKey))
                    )),
                  confirmForm =>
                    if (confirmForm.value.contains(true)) {
                      for {
                        _ <- Future.sequence(
                              journeyState.consents.map(c => invitationsService.rejectInvitation(c.invitationId)))
                      } yield Redirect(routes.ClientsMultiInvitationController.getMultiInvitationsDeclined(uid))
                    } else {
                      Future.successful(
                        Redirect(routes.ClientsMultiInvitationController.getMultiConfirmTerms(clientType, uid)))
                  }
                )
            }
          } yield result
        } else {
          Future successful Redirect(routes.ClientErrorController.incorrectClientType())
            .addingToSession("clientType" -> clientType)
        }
      }
  }

  def getMultiInvitationsDeclined(uid: String): Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAnyClient { (_, _) =>
      withAgencyNameAndConsents(uid, Rejected) { (agencyName, consents) =>
        journeyStateCache.get
          .map { journeyState =>
            {
              val cacheIds = journeyState.consents.map(_.serviceKey)
              val filteredServiceKeys = consents.filter(c => cacheIds.contains(c.serviceKey)).map(_.serviceKey).distinct
              Ok(invitation_declined(MultiInvitationDeclinedPageConfig(agencyName, filteredServiceKeys)))
            }
          }
      }.recoverWith {
        case ex: NotFoundException => targets.NotFoundInvitation
      }
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
    body: (String, Seq[ClientConsent]) => Future[Result])(implicit hc: HeaderCarrier): Future[Result] =
    for {
      invitations <- invitationsConnector.getAllClientInvitationsInfoForAgentAndStatus(uid, status)
      agencyName  <- getAgencyName(uid)
      consents = invitations.map(
        invitation =>
          ClientConsent(
            invitation.invitationId,
            invitation.expiryDate,
            Services.determineServiceMessageKey(invitation.invitationId),
            consent = false))
      result <- body(agencyName, consents)
    } yield result

}

object ClientsMultiInvitationController {

  val confirmTermsMultiForm: Form[ConfirmedTerms] =
    Form[ConfirmedTerms](
      mapping(
        "confirmedTerms.itsa" -> boolean,
        "confirmedTerms.afi"  -> boolean,
        "confirmedTerms.vat"  -> boolean
      )(ConfirmedTerms.apply)(ConfirmedTerms.unapply))

  def updateMultiInvitation(confirmedTerms: ConfirmedTerms)(
    item: ClientConsentsJourneyState): ClientConsentsJourneyState = {

    val hasConsent: String => Boolean = {
      case "itsa" => confirmedTerms.itsaConsent
      case "afi"  => confirmedTerms.afiConsent
      case "vat"  => confirmedTerms.vatConsent
    }

    item.copy(consents = item.consents.map(c => c.copy(consent = hasConsent(c.serviceKey))))
  }

}
