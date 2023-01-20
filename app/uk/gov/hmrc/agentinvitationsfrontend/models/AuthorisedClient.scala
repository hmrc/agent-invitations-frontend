/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.agentinvitationsfrontend.models

import uk.gov.hmrc.agentmtdidentifiers.model.Service
import uk.gov.hmrc.auth.core.AffinityGroup.{Individual, Organisation}
import uk.gov.hmrc.auth.core.{AffinityGroup, Enrolments}

import scala.collection.immutable.Set

case class AuthorisedClient(affinityGroup: AffinityGroup, enrolments: Enrolments) {

  def enrolmentCoverage: EnrolmentCoverage = {
    val enrolKeys: Set[String] = this.enrolments.enrolments.map(_.key)
    this.affinityGroup match {
      case Individual => {
        val coverage = enrolKeys.intersect(Services.supportedEnrolmentKeysFor(ClientType.Personal))
        if (coverage.size == Services.supportedEnrolmentKeysFor(ClientType.Personal).size) AllSupportedMTDEnrolments
        else if (coverage.isEmpty) NoSupportedMTDEnrolments
        else SomeSupportedMTDEnrolments
      }
      case Organisation => {
        val businessCoverage = enrolKeys.intersect(Services.supportedEnrolmentKeysFor(ClientType.Business))
        val trustOrEstateCoverage = enrolKeys.intersect(Services.supportedEnrolmentKeysFor(ClientType.Trust))
        if (businessCoverage.isEmpty) {
          if (trustOrEstateCoverage.isEmpty) NoSupportedMTDEnrolments
          else if (trustOrEstateCoverage.size == Services.supportedEnrolmentKeysFor(ClientType.Trust).size - 1) //not going to have both UTR and URN
            AllSupportedMTDEnrolments
          else SomeSupportedMTDEnrolments
        } else {
          organisationCoverageResultIgnoringIRV(enrolKeys)
        }
      }
      case e => throw new RuntimeException(s"client had unexpected Affinity Group: $e")
    }
  }

  private def organisationCoverageResultIgnoringIRV(enrolKeys: Set[String]): EnrolmentCoverage =
    Services.supportedEnrolmentKeysFor(ClientType.Business) diff enrolKeys match {
      case set if set == Set(Service.PersonalIncomeRecord.enrolmentKey) | set.isEmpty => AllSupportedMTDEnrolments
      case _                                                                          => SomeSupportedMTDEnrolments
    }
}
