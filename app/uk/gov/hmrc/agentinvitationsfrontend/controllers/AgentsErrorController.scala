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

import com.google.inject.Provider
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.AgentMultiAuthorisationJourneyState
import uk.gov.hmrc.agentinvitationsfrontend.services.{AgentMultiAuthorisationJourneyStateCache, CurrentAuthorisationRequestCache}
import uk.gov.hmrc.agentinvitationsfrontend.views.agents.{AllInvitationCreationFailedPageConfig, SomeInvitationCreationFailedPageConfig}
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.{active_authorisation_exists, invitation_creation_failed, not_matched}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class AgentsErrorController @Inject()(
  auditService: AuditService,
  journeyStateCache: AgentMultiAuthorisationJourneyStateCache,
  currentAuthorisationRequestCache: CurrentAuthorisationRequestCache,
  val messagesApi: play.api.i18n.MessagesApi,
  val env: Environment,
  val authConnector: AuthConnector,
  val withVerifiedPasscode: PasscodeVerification)(
  implicit val configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  ec: ExecutionContext)
    extends FrontendController with I18nSupport with AuthActions {

  val notMatched: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.fetch.map { aggregateOpt =>
        val aggregate = aggregateOpt.getOrElse(AgentMultiAuthorisationJourneyState("", Set.empty))
        Forbidden(not_matched(aggregate.requests.nonEmpty))
      }
    }
  }

  val allCreateAuthorisationFailed: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.get.map(cacheItem =>
        Ok(invitation_creation_failed(AllInvitationCreationFailedPageConfig(cacheItem.requests))))
    }
  }

  val someCreateAuthorisationFailed: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      journeyStateCache.get.map(cacheItem =>
        Ok(invitation_creation_failed(SomeInvitationCreationFailedPageConfig(cacheItem.requests))))
    }
  }

  val activeRelationshipExists: Action[AnyContent] = Action.async { implicit request =>
    withAuthorisedAsAgent { (_, _) =>
      for {
        journeyStateCacheNonEmpty <- journeyStateCache.fetch.map {
                                      case Some(cache) => cache.requests.nonEmpty
                                      case None        => false
                                    }
        currentCacheItem <- currentAuthorisationRequestCache.get
      } yield Ok(active_authorisation_exists(journeyStateCacheNonEmpty, currentCacheItem.service))
    }
  }

}
