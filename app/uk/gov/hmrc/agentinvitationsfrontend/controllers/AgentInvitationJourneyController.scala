package uk.gov.hmrc.agentinvitationsfrontend.controllers
import javax.inject.{Inject, Named, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Call, Result}
import play.api.{Configuration, Environment}
import uk.gov.hmrc.agentinvitationsfrontend.audit.AuditService
import uk.gov.hmrc.agentinvitationsfrontend.config.ExternalUrls
import uk.gov.hmrc.agentinvitationsfrontend.connectors.InvitationsConnector
import uk.gov.hmrc.agentinvitationsfrontend.journeys.AgentInvitationJourneyService
import uk.gov.hmrc.agentinvitationsfrontend.services._
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AgentInvitationJourneyController @Inject()(
  @Named("invitation.expiryDuration") expiryDuration: String,
  invitationsService: InvitationsService,
  invitationsConnector: InvitationsConnector,
  relationshipsService: RelationshipsService,
  auditService: AuditService,
  val env: Environment,
  val authConnector: AuthConnector,
  val continueUrlActions: ContinueUrlActions,
  val withVerifiedPasscode: PasscodeVerification,
  override val journeyService: AgentInvitationJourneyService)(
  implicit configuration: Configuration,
  val externalUrls: ExternalUrls,
  featureFlags: FeatureFlags,
  val messagesApi: play.api.i18n.MessagesApi,
  ec: ExecutionContext)
    extends BaseJourneyController(journeyService) with I18nSupport with AuthActions {

  import journeyService.model.{Error, State, States, Transitions}

  val agentsRootUrl: Call = routes.AgentsInvitationController.showClientType()

  val agentsRoot: Action[AnyContent] = simpleAction(Transitions.startJourney)

  override val handleError: Error => Future[Result] = {
    case error => Future.failed(throw new Exception(error.toString))
  }

  override val renderState: State => Result = {
    case States.Start => Redirect(agentsRootUrl)
  }

}
