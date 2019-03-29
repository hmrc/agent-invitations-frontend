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

package uk.gov.hmrc.agentinvitationsfrontend.controllers.retired

import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.controllers.{AuthActions, FeatureFlags, PasscodeVerification}
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentSession
import uk.gov.hmrc.agentinvitationsfrontend.repository.AgentSessionCache
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{AllInvitationCreationFailedPageConfig, CannotCreateRequestConfig, SomeInvitationCreationFailedPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class AgentsErrorController @Inject()(
  authActions: AuthActions,
  agentSessionCache: AgentSessionCache,
  val messagesApi: play.api.i18n.MessagesApi)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  ec: ExecutionContext)
    extends FrontendController with I18nSupport {
  import authActions._

  val notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.fetch.map { aggregateOpt =>
        val aggregate = aggregateOpt.getOrElse(AgentSession())
        Forbidden(
          not_matched(
            aggregate.requests.nonEmpty,
            routes.AgentsInvitationController.showIdentifyClient(),
            routes.AgentsInvitationController.showReviewAuthorisations()))
      }
    }
  }

  val allCreateAuthorisationFailed: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.hardGet.map(cacheItem =>
        Ok(invitation_creation_failed(AllInvitationCreationFailedPageConfig(cacheItem.requests))))
    }
  }

  val someCreateAuthorisationFailed: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.hardGet.map(cacheItem =>
        Ok(invitation_creation_failed(SomeInvitationCreationFailedPageConfig(cacheItem.requests))))
    }
  }

  val activeRelationshipExists: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      for {
        agentSession <- agentSessionCache.hardGet
      } yield
        Ok(
          active_authorisation_exists(
            agentSession.requests.nonEmpty,
            agentSession.service.getOrElse(""),
            agentSession.fromFastTrack,
            routes.AgentsInvitationController.showReviewAuthorisations(),
            routes.AgentsInvitationController.showClientType()
          ))
    }
  }

  val notAuthorised: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.get.map {
        case Right(mayBeSession) => Ok(not_authorised(mayBeSession.getOrElse(AgentSession()).service.getOrElse("")))
        case Left(_)             => Ok(not_authorised("")) //TODO
      }
    }
  }

  val cannotCreateRequest: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { _ =>
      agentSessionCache.fetch.map {
        case Some(session) =>
          val backLink =
            if (session.fromFastTrack)
              routes.AgentsFastTrackInvitationController.showCheckDetails().url
            else routes.AgentsInvitationController.showIdentifyClient().url

          Ok(
            cannot_create_request(
              CannotCreateRequestConfig(session.requests.nonEmpty, session.fromFastTrack, backLink)))
        case None =>
          Ok(
            cannot_create_request(
              CannotCreateRequestConfig(false, false, routes.AgentsInvitationController.showIdentifyClient().url)))
      }
    }
  }

}
