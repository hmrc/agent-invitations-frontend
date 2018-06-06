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

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import uk.gov.hmrc.agentmtdidentifiers.model.Vrn
import uk.gov.hmrc.domain.Nino

case class FastTrackInvitation (service: Option[String],
                                clientIdentifierType: Option[String],
                                clientIdentifier: Option[String],
                                postcode: Option[String],
                                vatRegDate: Option[String]) {


  val clientIdentifierTypeConversion: Option[String] = clientIdentifier match {
    case Some(clientId) =>{
      if(Nino.isValid(clientId)) Some("ni")
      else if (Vrn.isValid(clientId)) Some("vrn")
      else None
    }
    case _ => None
  }
}

object FastTrackInvitation {

  def apply(): FastTrackInvitation = FastTrackInvitation(None, None, None, None, None)
  def apply(service: String): FastTrackInvitation = FastTrackInvitation(Some(service),None,None,None,None)

  implicit val format = Json.format[FastTrackInvitation]

  implicit val reads: Reads[FastTrackInvitation] = {
    (
        (JsPath \ "service").readNullable[String] and
        (JsPath \ "clientIdentifierType").readNullable[String] and
        (JsPath \ "clientIdentifier").readNullable[String] and
        (JsPath \ "postcode").readNullable[String] and
        (JsPath \ "vatRegDate").readNullable[String])((a,b,c,d,e) => FastTrackInvitation(a,b,c,d,e))
  }
}