package streams

import scala.annotation.tailrec

object IndividualTask {
  def main(args: Array[String]) {
    println("hello stub")
    val range = (-250 to 25)
    range.foreach(println)
    val list = toList(range, countExpression2)
    list.foreach(println)
//    val listMethod = range filter countExpression.isDefinedAt map countExpression
  }

  val countExpression = new PartialFunction[Int, BigInt] {
    def apply(x: Int): BigInt = {
      @tailrec
      def iterateFact(x: Int, result: BigInt = 1): BigInt =
        if (x == 0) result
        else iterateFact(x - 1, result * x)

      if (x < 0) 0
      else if (x >= 0 && x <= 1) throw new IllegalArgumentException("function not defined on [0, 1]")
      else iterateFact(x)
    }

    def isDefinedAt(x: Int): Boolean = (x < 0 || x > 1)
  }

  val countExpression2: PartialFunction[Int, BigInt] = {
    case x: Int if (x < 0 || x > 1) => {
      def iterateFact(x: Int, result: BigInt = 1): BigInt =
        if (x == 0) result
        else iterateFact(x - 1, result * x)

      if (x < 0) 0
      else if (x >= 0 && x <= 1) throw new IllegalArgumentException("function not defined on [0, 1]")
      else iterateFact(x)
    }
  }

  def toList(range: Seq[Int], f: PartialFunction[Int, BigInt]): List[BigInt] = {
    for (x <- range.toList if f isDefinedAt x) yield f(x)
  }
}