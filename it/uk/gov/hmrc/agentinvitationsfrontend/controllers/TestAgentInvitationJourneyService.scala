package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import javax.inject.Singleton
import uk.gov.hmrc.agentinvitationsfrontend.journeys.{AgentInvitationJourneyModel, AgentInvitationJourneyService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class TestAgentInvitationJourneyService extends AgentInvitationJourneyService {

  @volatile
  private var state: Option[StateAndBreadcrumbs] = None

  def set(state: model.State, breadcrumbs: List[model.State])(implicit
    headerCarrier: HeaderCarrier,
    timeout: Duration,
    ec: ExecutionContext
  ): (AgentInvitationJourneyModel.State, List[AgentInvitationJourneyModel.State]) =
    Await.result(save((state, breadcrumbs)), timeout)

  def get: Option[StateAndBreadcrumbs] = state

  override protected def fetch(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[(model.State, List[model.State])]] =
    Future.successful(
      state
    )

  override protected def save(
    state: (model.State, List[model.State])
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[(model.State, List[model.State])] =
    Future {
      this.state = Some(state)
      state
    }
}

private class TestAgentInvitationJourneyModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[AgentInvitationJourneyService]).to(classOf[TestAgentInvitationJourneyService])
    ()
  }
}
