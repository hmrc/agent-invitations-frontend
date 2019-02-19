package journeys
import uk.gov.hmrc.agentinvitationsfrontend.journeys.{JourneyModel, PersistentJourneyService}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyServiceSpec extends UnitSpec {

  implicit val hc = new HeaderCarrier()

  val testService = new PersistentJourneyService with TestStorage[(String, List[String])] {
    val model = new TestJourneyModel
  }

  "PersistentJourneyService" should {
    "apply transition and return new state keeping account of the breadcrumbs" in {
      await(testService.save(("foo", Nil)))
      await(testService.apply(testService.model.Transitions.append("bar")))
      await(testService.fetch) shouldBe Some(("foobar", List("foo")))
      await(testService.apply(testService.model.Transitions.reverse))
      await(testService.fetch) shouldBe Some(("raboof", List("foobar", "foo")))
      await(testService.apply(testService.model.Transitions.append("bar")))
      await(testService.fetch) shouldBe Some(("raboofbar", List("raboof", "foobar", "foo")))
      await(testService.apply(testService.model.Transitions.reverse))
      await(testService.fetch) shouldBe Some(("rabfoobar", List("raboofbar", "raboof", "foobar", "foo")))
      await(testService.apply(testService.model.Transitions.append("foo")))
      await(testService.fetch) shouldBe Some(
        ("rabfoobarfoo", List("rabfoobar", "raboofbar", "raboof", "foobar", "foo")))
      await(testService.apply(testService.model.Transitions.reverse))
      await(testService.fetch) shouldBe Some(
        ("oofraboofbar", List("rabfoobarfoo", "rabfoobar", "raboofbar", "raboof", "foobar", "foo")))
      await(testService.apply(testService.model.Transitions.replace("o", "x")))
      await(testService.fetch) shouldBe Some(
        ("xxfrabxxfbar", List("oofraboofbar", "rabfoobarfoo", "rabfoobar", "raboofbar", "raboof", "foobar", "foo")))
      await(testService.apply(testService.model.Transitions.replace("xx", "o")))
      await(testService.fetch) shouldBe Some(
        (
          "ofrabofbar",
          List("xxfrabxxfbar", "oofraboofbar", "rabfoobarfoo", "rabfoobar", "raboofbar", "raboof", "foobar", "foo")))
      await(testService.apply(testService.model.Transitions.reverse))
      await(testService.fetch) shouldBe Some(
        (
          "rabfobarfo",
          List(
            "ofrabofbar",
            "xxfrabxxfbar",
            "oofraboofbar",
            "rabfoobarfoo",
            "rabfoobar",
            "raboofbar",
            "raboof",
            "foobar",
            "foo")))
      await(testService.apply(testService.model.Transitions.append("bar")))
      await(testService.fetch) shouldBe Some(
        (
          "rabfobarfobar",
          List(
            "rabfobarfo",
            "ofrabofbar",
            "xxfrabxxfbar",
            "oofraboofbar",
            "rabfoobarfoo",
            "rabfoobar",
            "raboofbar",
            "raboof",
            "foobar",
            "foo")))
      await(testService.apply(testService.model.Transitions.reverse))
      await(testService.fetch) shouldBe Some(
        (
          "rabofrabofbar",
          List(
            "rabfobarfobar",
            "rabfobarfo",
            "ofrabofbar",
            "xxfrabxxfbar",
            "oofraboofbar",
            "rabfoobarfoo",
            "rabfoobar",
            "raboofbar",
            "raboof",
            "foobar"))) // foo is removed because we keep only 10 last states
    }
  }

}

class TestJourneyModel extends JourneyModel {

  type State = String
  type Error = Int

  /** Where your journey starts by default */
  override def root: State = "start"

  /** Error to report an attempt to make invalid transition */
  override def transitionNotAllowed(state: State, breadcrumbs: List[State], transition: Transition): Error = -1

  /** Converts or wraps exception thrown during transition into an Error */
  override def errorFor(ex: Exception): Error = -2

  object Transitions {

    def append(suffix: String) = Transition {
      case s => goto(s + suffix)
    }

    def reverse = Transition {
      case s => goto(s.reverse)
    }

    def replace(a: String, b: String) = Transition {
      case s => goto(s.replaceAllLiterally(a, b))
    }

  }
}
