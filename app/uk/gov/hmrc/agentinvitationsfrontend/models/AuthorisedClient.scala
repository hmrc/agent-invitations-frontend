/*
 * Copyright 2020 HM Revenue & Customs
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

import uk.gov.hmrc.agentinvitationsfrontend.models.EnrolmentCoverage.{allSupportedServicesForBusiness, allSupportedServicesForIndividual, allSupportedServicesForTrustOrEstate}
import uk.gov.hmrc.auth.core.AffinityGroup.{Individual, Organisation}
import uk.gov.hmrc.auth.core.{AffinityGroup, Enrolments}

case class AuthorisedClient(affinityGroup: AffinityGroup, enrolments: Enrolments) {

  def enrolmentCoverage: EnrolmentCoverage = {
    val enrolKeys: Set[String] = this.enrolments.enrolments.map(_.key)
    this.affinityGroup match {
      case Individual => {
        val coverage = enrolKeys.intersect(allSupportedServicesForIndividual)
        if (coverage.size == allSupportedServicesForIndividual.size) AllSupportedMTDEnrolments
        else if (coverage.isEmpty) NoSupportedMTDEnrolments
        else SomeSupportedMTDEnrolments
      }
      case Organisation => {
        val businessCoverage = enrolKeys.intersect(allSupportedServicesForBusiness)
        val trustOrEstateCoverage = enrolKeys.intersect(allSupportedServicesForTrustOrEstate)
        if (businessCoverage.isEmpty) {
          if (trustOrEstateCoverage.isEmpty) NoSupportedMTDEnrolments
          else if (trustOrEstateCoverage.size == allSupportedServicesForTrustOrEstate.size) AllSupportedMTDEnrolments
          else SomeSupportedMTDEnrolments
        } else {
          if (businessCoverage.size == allSupportedServicesForBusiness.size) AllSupportedMTDEnrolments
          else SomeSupportedMTDEnrolments
        }
      }
      case e => throw new RuntimeException(s"client had unexpected Affinity Group: $e")
    }
  }
}
