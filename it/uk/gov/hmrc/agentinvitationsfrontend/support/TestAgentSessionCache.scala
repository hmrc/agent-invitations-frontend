package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.models.AgentSession
import uk.gov.hmrc.agentinvitationsfrontend.services.AgentSessionCache

class TestAgentSessionCache extends AgentSessionCache with TestCache[AgentSession]
