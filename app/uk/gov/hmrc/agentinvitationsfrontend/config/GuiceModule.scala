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

package uk.gov.hmrc.agentinvitationsfrontend.config

import java.net.URL
import javax.inject.Provider

import com.google.inject.AbstractModule
import com.google.inject.binder.ScopedBindingBuilder
import com.google.inject.name.Names
import play.api.Mode.Mode
import play.api.{Configuration, Environment, Logger, LoggerLike}
import uk.gov.hmrc.agentinvitationsfrontend.{FrontendAuthConnector, WSHttp}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.HttpGet
import uk.gov.hmrc.play.config.inject.ServicesConfig

class GuiceModule(val environment: Environment, val config: Configuration) extends AbstractModule with ServicesConfig {

  override def configure(): Unit = {
    bind(classOf[HttpGet]).toInstance(WSHttp)
    bind(classOf[AuthConnector]).toInstance(FrontendAuthConnector)
    bind(classOf[LoggerLike]).toInstance(Logger)
    bindBaseUrl("auth")
    bindBaseUrl("agent-client-authorisation")
  }

  trait ConfigPropertyType[A] {
    def bindConfigProperty(clazz: Class[A])(propertyName: String): ScopedBindingBuilder
  }

  object ConfigPropertyType {
    implicit val stringConfigProperty = new ConfigPropertyType[String] {
      def bindConfigProperty(clazz: Class[String])(propertyName: String): ScopedBindingBuilder =
        bind(clazz).annotatedWith(Names.named(s"$propertyName")).toProvider(new StringConfigPropertyProvider(propertyName))

      private class StringConfigPropertyProvider(propertyName: String) extends Provider[String] {
        override lazy val get = getConfString(propertyName, throw new RuntimeException(s"No configuration value found for '$propertyName'"))

        def getConfString(confKey: String, defString: => String) = {
          runModeConfiguration.getString(s"$env.$confKey").getOrElse(defString)
        }
      }
    }

    implicit val intConfigProperty = new ConfigPropertyType[Int] {
      def bindConfigProperty(clazz: Class[Int])(propertyName: String): ScopedBindingBuilder =
        bind(clazz).annotatedWith(Names.named(s"$propertyName")).toProvider(new IntConfigPropertyProvider(propertyName))

      private class IntConfigPropertyProvider(propertyName: String) extends Provider[Int] {
        override lazy val get = getConfString(propertyName, throw new RuntimeException(s"No configuration value found for '$propertyName'"))

        def getConfString(confKey: String, defInt: => Int) = {
          runModeConfiguration.getInt(s"$env.$confKey").getOrElse(defInt)
        }
      }
    }
  }

  object ConfigProperty {
    def bindConfigProperty[A](clazz: Class[A])(propertyName: String)(implicit ct: ConfigPropertyType[A]): ScopedBindingBuilder =
      ct.bindConfigProperty(clazz)(propertyName)
  }

  private def bindBaseUrl(serviceName: String) =
    bind(classOf[URL]).annotatedWith(Names.named(s"$serviceName-baseUrl")).toProvider(new BaseUrlProvider(serviceName))

  private class BaseUrlProvider(serviceName: String) extends Provider[URL] {
    override lazy val get = new URL(baseUrl(serviceName))
  }
}
