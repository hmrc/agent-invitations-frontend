package uk.gov.hmrc.agentinvitationsfrontend.support

import uk.gov.hmrc.agentinvitationsfrontend.models.CurrentAuthorisationRequest
import uk.gov.hmrc.agentinvitationsfrontend.services.CurrentAuthorisationRequestCache
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class TestCurrentAuthorisationRequestCache extends CurrentAuthorisationRequestCache with TestCache[CurrentAuthorisationRequest]
