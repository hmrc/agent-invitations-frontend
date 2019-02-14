package uk.gov.hmrc.agentinvitationsfrontend.journeys
import play.api.libs.json.{Json, _}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._

object AgentInvitationJourneyStateFormats extends JsonStateFormats[State] {

  val selectServiceFormat = Json.format[SelectService]

  override val serializeStateProperties: PartialFunction[State, JsValue] = {
    case s: SelectService => selectServiceFormat.writes(s)
  }

  override def deserializeState(stateName: String, properties: JsValue): JsResult[State] = stateName match {
    case "UnknownState"     => JsSuccess(UnknownState)
    case "Start"            => JsSuccess(Start)
    case "SelectClientType" => JsSuccess(SelectClientType)
    case "SelectService"    => selectServiceFormat.reads(properties)
    case _                  => JsError(s"Unknown state name $stateName")
  }

}
