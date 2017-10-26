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

package uk.gov.hmrc.agentinvitationsfrontend.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.agentinvitationsfrontend.support.WireMockSupport

trait AuthStubs {
  me: WireMockSupport =>

  case class Enrolment(serviceName: String, identifierName: String, identifierValue: String)

  def givenUnauthorisedWith(mdtpDetail: String): AuthStubs = {
    stubFor(post(urlEqualTo("/auth/authorise"))
      .willReturn(aResponse()
        .withStatus(401)
        .withHeader("WWW-Authenticate", s"""MDTP detail="${mdtpDetail}"""")
      )
    )
    this
  }

  def givenAuthorisedFor(enrolment: Option[Enrolment], affinityGroup: String): AuthStubs = {
    stubFor(post(urlEqualTo("/auth/authorise")).atPriority(1)
      .withRequestBody(
        equalToJson(
          s"""
             |{
             |  "authorise": [
             |    { "affinityGroup": "$affinityGroup" },
             |    ${enrolment.map(e => s"""{ "identifiers":[], "state":"Activated", "enrolment": "${e.serviceName}" },""").getOrElse("")}
             |    {
             |      "authProviders": ["GovernmentGateway"]
             |    }
             |  ],
             |  "retrieve":["authorisedEnrolments"]
             |}
           """.stripMargin, true, true))
      .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type","application/json")
        .withBody(
          s"""
            |{
            |${enrolment.map(e => s""""authorisedEnrolments": [{ "key":"${e.serviceName}", "identifiers": [{"key":"${e.identifierName}", "value": "${e.identifierValue}"}] }]""").getOrElse("")}
            |}
          """.stripMargin))
    )

    //{"authorise":[{"affinityGroup":"Agent"},{"identifiers":[],"state":"Activated","enrolment":"HMRC-AS-AGENT"},{"authProviders":["GovernmentGateway"]}],"retrieve":["authorisedEnrolments"]}

    stubFor(post(urlEqualTo("/auth/authorise")).atPriority(2)
      .willReturn(aResponse()
        .withStatus(401)
        .withHeader("WWW-Authenticate", "MDTP detail=\"InsufficientEnrolments\"")
      )
    )
    this
  }

  def verifyAuthoriseAttempt() = {
    verify(1, postRequestedFor(urlEqualTo("/auth/authorise")))
  }
}