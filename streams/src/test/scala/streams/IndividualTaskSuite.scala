package streams

class IndividualTaskSuite extends munit.FunSuite {

  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds
}
