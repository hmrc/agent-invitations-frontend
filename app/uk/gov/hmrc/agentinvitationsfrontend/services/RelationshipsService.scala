package uk.gov.hmrc.agentinvitationsfrontend.services
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.agentinvitationsfrontend.connectors.{PirRelationshipConnector, RelationshipsConnector}
import uk.gov.hmrc.agentmtdidentifiers.model.{Arn, Vrn}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

@Singleton
class RelationshipsService @Inject()(
  relationshipsConnector: RelationshipsConnector,
  pirRelationshipConnector: PirRelationshipConnector) {

  def hasActiveRelationshipFor(arn: Arn, clientId: String, service: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = {
    val relationships = service match {
      case "HMRC-MTD-IT" => {
        relationshipsConnector.getItsaRelationshipForAgent(Nino(clientId))
      }
      case "HMRC-MTD-VAT"           => relationshipsConnector.getVatRelationshipForAgent(Vrn(clientId))
      case "PERSONAL-INCOME-RECORD" => pirRelationshipConnector.getPirRelationshipForAgent(arn, Nino(clientId))
    }
    val x = relationships.map(_.nonEmpty)
    println(s"^^^^^^^^ $x")
    x
  }

}
