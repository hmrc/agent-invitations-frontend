package uk.gov.hmrc.agentinvitationsfrontend.controllers

import com.google.inject.AbstractModule
import javax.inject.Singleton
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentLedDeauthJourneyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class TestAgentLedDeauthJourneyService extends AgentLedDeauthJourneyService {

  @volatile
  private var state: Option[StateAndBreadcrumbs] = None

  def set(state: model.State, breadcrumbs: List[model.State])(
    implicit headerCarrier: HeaderCarrier,
    timeout: Duration,
    ec: ExecutionContext): Unit =
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

  override def clear(implicit rc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future {
      this.state = None
    }
}

private class TestAgentLedDeauthJourneyModule extends AbstractModule {
  override def configure(): Unit =
    bind(classOf[AgentLedDeauthJourneyService]).to(classOf[TestAgentLedDeauthJourneyService])
}
