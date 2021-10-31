package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

trait IndividualTask {
  lazy val posGen: Gen[Int] = choose(2, 100)
  lazy val uncertaintyGen: Gen[Int] = oneOf(const(0), const(1))

  @tailrec
  final def iterateFact(x: Int, result: BigInt = 1): BigInt =
    if (x == 0) result
    else iterateFact(x - 1, result * x)

  val countExpression: PartialFunction[Int, BigInt] = {
    case x: Int if (x < 0 || x > 1) => {
      if (x < 0) 0
      else if (x >= 0 && x <= 1) throw new IllegalArgumentException("function not defined on [0, 1]")
      else iterateFact(x)
    }
  }

  val liftedCE: Int => Option[BigInt] = countExpression.lift
}

object QuickCheckIndividualTask extends Properties("IndividualTask") with IndividualTask {
  property("all numbers less than zero must return zero") = forAll { (x: Int) =>
    x < 0 ==> {
      liftedCE(x).contains(0)
    }
  }

  property("in the area of uncertainty returns None") = forAll(uncertaintyGen)((x: Int) => liftedCE(x).isEmpty)

  property("factorial should be returned for all positive numbers > 1") = forAll(posGen) { (x: Int) =>
    liftedCE(x).contains(iterateFact(x))
  }
}
