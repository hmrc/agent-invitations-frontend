package uk.gov.hmrc.agentinvitationsfrontend.support

import org.scalatest.{BeforeAndAfterEach, Suite}
import uk.gov.hmrc.mongo.MongoSpecSupport

import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, _}
import scala.concurrent.duration._
import scala.language.postfixOps

trait MongoSupport extends MongoSpecSupport with BeforeAndAfterEach {
  me: Suite =>

  def dropMongoDb()(implicit ec: ExecutionContext = global): Unit =
    Await.result(mongo().drop(), 5 seconds)
}
