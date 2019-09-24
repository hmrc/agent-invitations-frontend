package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import javax.inject.Singleton
import uk.gov.hmrc.agentinvitationsfrontend.journeys.{AgentInvitationFastTrackJourneyModel, AgentInvitationFastTrackJourneyService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class TestAgentInvitationFastTrackJourneyService extends AgentInvitationFastTrackJourneyService {

  @volatile
  private var state: Option[StateAndBreadcrumbs] = None

  def set(state: model.State, breadcrumbs: List[model.State])(
    implicit headerCarrier: HeaderCarrier,
    timeout: Duration,
    ec: ExecutionContext): (AgentInvitationFastTrackJourneyModel.State, List[AgentInvitationFastTrackJourneyModel.State]) =
    Await.result(save((state, breadcrumbs)), timeout)

  def get: Option[StateAndBreadcrumbs] = state

  override protected def fetch(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[(model.State, List[model.State])]] = Future.successful(
    state
  )

  override protected def save(state: (model.State, List[model.State]))(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[(model.State, List[model.State])] =
    Future {
      this.state = Some(state)
      state
    }

  override def clear(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    this.state = None
    super.clear(hc, ec)
  }

}

private class TestAgentInvitationFastTrackJourneyModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[AgentInvitationFastTrackJourneyService]).to(classOf[TestAgentInvitationFastTrackJourneyService])
    ()
  }
}
