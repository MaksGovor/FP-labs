package streams

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("hello stub")
    val range = (-25 to 25)
    val list = toList(range, countExpression)

    val mappedList = list.map(x => x / 5)
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

  def toList(range: Seq[Int], f: PartialFunction[Int, BigInt]): List[BigInt] = {
    for (x <- range.toList if f isDefinedAt x) yield f(x)
  }
}