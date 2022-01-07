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

package uk.gov.hmrc.agentinvitationsfrontend.views.agents

import javax.inject.Inject
import play.api.i18n.{Messages, MessagesApi}
import play.twirl.api.Html
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.NotSignedUpPageUrls
import uk.gov.hmrc.agentinvitationsfrontend.models.Services._
import uk.gov.hmrc.agentinvitationsfrontend.views.html.agents.not_signed_up_partial

class NotSignedUpPageConfig @Inject()(notSignedUpPartial: not_signed_up_partial)(implicit externalUrls: ExternalUrls, messages: MessagesApi) {

  def render(serviceId: String)(implicit externalUrls: ExternalUrls, messages: Messages): Option[Html] = {
    val urls = serviceId match {
      case HMRCMTDVAT => Some(NotSignedUpPageUrls(externalUrls.guidanceUrlVatExisting, externalUrls.guidanceUrlVatNew))
      case HMRCMTDIT  => Some(NotSignedUpPageUrls(externalUrls.guidanceUrlSaExisting, externalUrls.guidanceUrlSaNew))
      case _          => None
    }
    urls.map(v => notSignedUpPartial(serviceId, v))
  }
}
