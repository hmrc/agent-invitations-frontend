package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.models.ClientConsentsJourneyState
import uk.gov.hmrc.agentinvitationsfrontend.services.ClientConsentsJourneyStateCache

class TestClientConsentsJourneyStateCache extends ClientConsentsJourneyStateCache with TestCache[ClientConsentsJourneyState]
