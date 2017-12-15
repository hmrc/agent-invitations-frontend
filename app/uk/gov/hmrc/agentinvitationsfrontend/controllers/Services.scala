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

package uk.gov.hmrc.agentinvitationsfrontend.controllers

import uk.gov.hmrc.agentmtdidentifiers.model.InvitationId

trait Service
case class ValidService(serviceName: String, serviceIdentifier:String, apiIdentifier: String, messageKey: String) extends Service
case object InvalidService extends Service

object Services {

  val ITSA = "itsa"
  val AFI = "afi"

  def determineService(invitationId: InvitationId): Service = {
    invitationId.value.head match {
      case 'A' => ValidService("HMRC-MTD-IT", "MTDITID", "MTDITID", ITSA)
      case 'B' => ValidService("HMRC-NI", "NINO", "NI", AFI)
      case _ => InvalidService
    }
  }
}