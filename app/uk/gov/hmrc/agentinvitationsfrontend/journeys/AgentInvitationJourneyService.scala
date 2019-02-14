package uk.gov.hmrc.agentinvitationsfrontend.journeys
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.libs.json._
import uk.gov.hmrc.agentinvitationsfrontend.services.Cache
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.cache.client.SessionCache

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[KeystoreCachedAgentInvitationJourneyService])
trait AgentInvitationJourneyService extends PersistentJourneyService {

  override val model = AgentInvitationJourneyModel

  implicit val formats: Format[model.State] = AgentInvitationJourneyStateFormats.formats
}

@Singleton
class KeystoreCachedAgentInvitationJourneyService @Inject()(session: SessionCache)
    extends AgentInvitationJourneyService with Cache[AgentInvitationJourneyModel.State] {

  import model.State

  val id = "agent-invitation-journey"

  def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[model.State]] =
    session.fetchAndGetEntry[State](id)

  def fetchAndClear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[State]] =
    for {
      entry <- session.fetchAndGetEntry[State](id)
      _     <- session.cache(id, model.root)
    } yield entry

  def save(agentAuthorisationInput: State)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[State] =
    session.cache(id, agentAuthorisationInput).map(_ => agentAuthorisationInput)

}
