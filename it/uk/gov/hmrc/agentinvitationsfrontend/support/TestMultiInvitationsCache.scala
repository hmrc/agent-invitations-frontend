package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.models.{MultiInvitationsCache, MultiInvitationsCacheItem}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class TestMultiInvitationsCache extends MultiInvitationsCache with TestCache[MultiInvitationsCacheItem]
