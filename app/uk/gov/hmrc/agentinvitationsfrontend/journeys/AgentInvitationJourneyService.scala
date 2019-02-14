package uk.gov.hmrc.agentinvitationsfrontend.journeys
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.libs.json._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[KeystoreCachedAgentInvitationJourneyService])
trait AgentInvitationJourneyService extends PersistentJourneyService {

  override val model = AgentInvitationJourneyModel
}

@Singleton
class KeystoreCachedAgentInvitationJourneyService @Inject()(session: SessionCache)
    extends AgentInvitationJourneyService {

  import model.State

  val id = "agent-invitation-journey"

  implicit val formats: Format[model.State] = AgentInvitationJourneyStateFormats.formats

  protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[State]] =
    session.fetchAndGetEntry[State](id)

  protected def save(agentAuthorisationInput: State)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[State] =
    session.cache(id, agentAuthorisationInput).map(_ => agentAuthorisationInput)

}
