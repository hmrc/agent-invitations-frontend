package uk.gov.hmrc.agentinvitationsfrontend.support

import org.scalatest.{BeforeAndAfterEach, Suite}
import uk.gov.hmrc.mongo.MongoSpecSupport

import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, _}
import scala.concurrent.duration._
import scala.language.postfixOps

trait MongoSupport extends MongoSpecSupport with BeforeAndAfterEach {
  me: Suite =>

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def mongoConfiguration =
    Map(
      "mongodb.uri" -> "mongodb://localhost:27017/agent-invitations-frontend?rm.monitorRefreshMS=1000&rm.failover=default")

  def dropMongoDb()(implicit ec: ExecutionContext = global): Unit =
    Await.result(mongo().drop(), 5 seconds)
}
