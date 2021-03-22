/*
 * Copyright 2021 HM Revenue & Customs
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

import uk.gov.hmrc.agentinvitationsfrontend.models.Services.{allSupportedEnrolmentKeysForBusiness, allSupportedEnrolmentKeysForIndividual, allSupportedEnrolmentKeysForTrustOrEstate}
import uk.gov.hmrc.auth.core.AffinityGroup.{Individual, Organisation}
import uk.gov.hmrc.auth.core.{AffinityGroup, Enrolments}

case class AuthorisedClient(affinityGroup: AffinityGroup, enrolments: Enrolments) {

  def enrolmentCoverage: EnrolmentCoverage = {
    val enrolKeys: Set[String] = this.enrolments.enrolments.map(_.key)
    this.affinityGroup match {
      case Individual => {
        val coverage = enrolKeys.intersect(allSupportedEnrolmentKeysForIndividual)
        if (coverage.size == allSupportedEnrolmentKeysForIndividual.size) AllSupportedMTDEnrolments
        else if (coverage.isEmpty) NoSupportedMTDEnrolments
        else SomeSupportedMTDEnrolments
      }
      case Organisation => {
        val businessCoverage = enrolKeys.intersect(allSupportedEnrolmentKeysForBusiness)
        val trustOrEstateCoverage = enrolKeys.intersect(allSupportedEnrolmentKeysForTrustOrEstate)
        if (businessCoverage.isEmpty) {
          if (trustOrEstateCoverage.isEmpty) NoSupportedMTDEnrolments
          else if (trustOrEstateCoverage.size == allSupportedEnrolmentKeysForTrustOrEstate.size - 1) //not going to have both UTR and URN
            AllSupportedMTDEnrolments
          else SomeSupportedMTDEnrolments
        } else {
          if (businessCoverage.size == allSupportedEnrolmentKeysForBusiness.size) AllSupportedMTDEnrolments
          else SomeSupportedMTDEnrolments
        }
      }
      case e => throw new RuntimeException(s"client had unexpected Affinity Group: $e")
    }
  }
}
