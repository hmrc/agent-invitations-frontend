package journeys
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.State
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyModel.States._
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyStateFormats
import uk.gov.hmrc.play.test.UnitSpec

class AgentInvitationJourneyStateFormatsSpec extends UnitSpec {

  implicit val formats: Format[State] = AgentInvitationJourneyStateFormats.formats

  "AgentsInvitationJourneyFormats" should {
    "serialize and deserialize state" when {
      "Start" in {
        Json.toJson(Start) shouldBe Json.obj("state" -> "Start")
        Json.parse("""{"state":"Start"}""").as[State] shouldBe Start
      }
      "SelectClientType" in {
        Json.toJson(SelectClientType) shouldBe Json.obj("state" -> "SelectClientType")
        Json.parse("""{"state":"SelectClientType"}""").as[State] shouldBe SelectClientType
      }
      "SelectService" in {
        Json.toJson(SelectService("person")) shouldBe Json
          .obj("state" -> "SelectService", "properties" -> Json.obj("clientType" -> "person"))
        Json
          .parse("""{"state":"SelectService", "properties": {"clientType": "person"}}""")
          .as[State] shouldBe SelectService("person")
        Json
          .parse("""{"state":"SelectService", "properties": {"clientType": "organisation"}}""")
          .as[State] shouldBe SelectService("organisation")
      }
    }

  }

}
