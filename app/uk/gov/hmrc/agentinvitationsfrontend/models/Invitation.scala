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

import play.api.libs.json.{Format, _}
import uk.gov.hmrc.agentmtdidentifiers.model.{CgtRef, Utr, Vrn}
import uk.gov.hmrc.domain.{Nino, TaxIdentifier}

sealed trait Invitation {
  val clientType: Option[ClientType]

  val service: String

  val clientIdentifier: TaxIdentifier

  val clientIdentifierType: String

  val clientId: String = clientIdentifier.value
}

object Invitation {
  def apply(clientType: Option[ClientType], service: String, clientIdentifier: String, knownFact: String): Invitation =
    service match {
      case Services.HMRCMTDIT  => ItsaInvitation(Nino(clientIdentifier))
      case Services.HMRCMTDVAT => VatInvitation(clientType, Vrn(clientIdentifier))
      case Services.HMRCPIR    => PirInvitation(Nino(clientIdentifier))
      case Services.TRUST      => TrustInvitation(Utr(clientIdentifier))
      case Services.HMRCCGTPD  => CgtInvitation(CgtRef(clientIdentifier), clientType)
    }

  implicit val format: Format[Invitation] = new Format[Invitation] {

    override def reads(json: JsValue): JsResult[Invitation] = {
      val t = (json \ "type").as[String]
      t match {
        case "ItsaInvitation"  => JsSuccess((json \ "data").as[ItsaInvitation])
        case "PirInvitation"   => JsSuccess((json \ "data").as[PirInvitation])
        case "VatInvitation"   => JsSuccess((json \ "data").as[VatInvitation])
        case "TrustInvitation" => JsSuccess((json \ "data").as[TrustInvitation])
        case "CgtInvitation"   => JsSuccess((json \ "data").as[CgtInvitation])
        case _                 => JsError(s"invalid json type for parsing invitation object, type=$t")
      }
    }

    override def writes(invitation: Invitation): JsValue = {
      val toJson = {
        Json.parse(s""" {
                      |"clientType": "${invitation.clientType.getOrElse(throw new RuntimeException("missing clientType from the invitation"))}",
                      |"service": "${invitation.service}",
                      |"clientIdentifier": "${invitation.clientIdentifier.value}",
                      |"clientIdentifierType": "${invitation.clientIdentifierType}"
                      |}""".stripMargin)
      }

      Json.obj("type" -> invitation.getClass.getSimpleName, "data" -> toJson)
    }
  }
}

case class ItsaInvitation(
  clientIdentifier: Nino,
  clientType: Option[ClientType] = Some(ClientType.personal),
  service: String = Services.HMRCMTDIT,
  clientIdentifierType: String = "ni")
    extends Invitation

object ItsaInvitation {
  implicit val format: Format[ItsaInvitation] = Json.format[ItsaInvitation]
}

case class PirInvitation(
  clientIdentifier: Nino,
  clientType: Option[ClientType] = Some(ClientType.personal),
  service: String = Services.HMRCPIR,
  clientIdentifierType: String = "ni")
    extends Invitation

object PirInvitation {
  implicit val format: Format[PirInvitation] = Json.format[PirInvitation]
}

case class VatInvitation(
  clientType: Option[ClientType],
  clientIdentifier: Vrn,
  service: String = Services.HMRCMTDVAT,
  clientIdentifierType: String = "vrn")
    extends Invitation

object VatInvitation {
  implicit val format: Format[VatInvitation] = Json.format[VatInvitation]
}

case class TrustInvitation(
  clientIdentifier: Utr,
  clientType: Option[ClientType] = Some(ClientType.business),
  service: String = Services.TRUST,
  clientIdentifierType: String = "utr")
    extends Invitation

object TrustInvitation {
  implicit val format: Format[TrustInvitation] = Json.format[TrustInvitation]
}

case class CgtInvitation(
  clientIdentifier: CgtRef,
  clientType: Option[ClientType],
  service: String = Services.HMRCCGTPD,
  clientIdentifierType: String = "CGTPDRef")
    extends Invitation

object CgtInvitation {
  implicit val format: Format[CgtInvitation] = Json.format
}
