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

sealed trait EnrolmentCoverage {
  val str: String
}

case object AllSupportedMTDEnrolments extends EnrolmentCoverage {
  override val str = "all supported MTD enrolments"
}

case object SomeSupportedMTDEnrolments extends EnrolmentCoverage {
  override val str = "some supported MTD enrolments"
}

case object NoSupportedMTDEnrolments extends EnrolmentCoverage {
  override val str = "no supported MTD Enrolments"
}

object EnrolmentCoverage {

  val HMRCMTDIT = "HMRC-MTD-IT"
  val HMRCPIR = "PERSONAL-INCOME-RECORD"
  val HMRCMTDVAT = "HMRC-MTD-VAT"
  val TRUST = "HMRC-TERS-ORG"
  val HMRCCGTPD = "HMRC-CGT-PD"
  val HMRCNI = "HMRC-NI"

  val allSupportedServicesForIndividual = Set(HMRCMTDIT, HMRCMTDVAT, HMRCCGTPD, HMRCNI)
  val allSupportedServicesForBusiness = Set(HMRCMTDVAT)
  val allSupportedServicesForTrustOrEstate = Set(HMRCCGTPD, TRUST)

}
