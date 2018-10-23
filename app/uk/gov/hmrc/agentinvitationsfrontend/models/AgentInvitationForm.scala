/*
 * Copyright 2018 HM Revenue & Customs
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

trait AgentInvitationForm {

  def clientType: String
  def service: String
  def clientIdentifier: Option[String]

}

case class UserInputNinoAndPostcode(
  clientType: String,
  service: String,
  clientIdentifier: Option[String],
  postcode: Option[String])
    extends AgentInvitationForm

case class UserInputVrnAndRegDate(
  clientType: String,
  service: String,
  clientIdentifier: Option[String],
  registrationDate: Option[String])
    extends AgentInvitationForm

case class UserInputNinoAndDob(
  clientType: String,
  service: String,
  clientIdentifier: Option[String],
  dob: Option[String])
    extends AgentInvitationForm
