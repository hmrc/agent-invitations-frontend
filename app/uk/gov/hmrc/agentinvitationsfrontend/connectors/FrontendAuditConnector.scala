/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.connectors

import javax.inject.{Inject, Singleton}

import play.api.Configuration
import uk.gov.hmrc.play.audit.http.config.{AuditingConfig, BaseUri, Consumer}
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

@Singleton
class FrontendAuditConnector @Inject() (configuration: Configuration) extends AuditConnector {

  override lazy val auditingConfig = loadAuditingConfig(configuration.getConfig("auditing"))

  private def loadAuditingConfig(config: Option[Configuration]) = config.map { c =>
    val enabled = c.getBoolean("enabled").getOrElse(true)
    if (enabled) {
      AuditingConfig(
        enabled = enabled,
        consumer = Some(c.getConfig("consumer").map { con =>
          Consumer(
            baseUri = con.getConfig("baseUri").map { uri =>
              BaseUri(
                host = uri.getString("host").getOrElse(throw new Exception("Missing consumer host for auditing")),
                port = uri.getInt("port").getOrElse(throw new Exception("Missing consumer port for auditing")),
                protocol = uri.getString("protocol").getOrElse("http"))
            }.getOrElse(throw new Exception("Missing consumer baseUri for auditing")))
        }.getOrElse(throw new Exception("Missing consumer configuration for auditing"))))
    } else {
      AuditingConfig(consumer = None, enabled = false)
    }
  }.getOrElse(throw new Exception("Missing auditing configuration"))
}
