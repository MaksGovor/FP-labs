package scalashop

import java.util.concurrent.{ConcurrentSkipListSet, TimeUnit}
import scala.annotation.tailrec
import scala.collection._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSet
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Try, Success, Failure}

object IndividualTask {

  implicit val globalExecutionContext: ExecutionContext = ExecutionContext.global

  def main(args: Array[String]):  Unit = {
    val rangeT = -250 to 25
    val dst = new Array[Option[BigInt]](rangeT.length)
    val numTasks = 5
    // Using task
//    countIntervalPar(rangeT, dst, numTasks)
//    dst.foreach(println)

    // Using parallel collections
//    val resParCol = mapCountInterval(rangeT.par.toSet)
//    resParCol.toArray.foreach(println)

    // Using Futures
//    val maxWaitTime: FiniteDuration = Duration(5, TimeUnit.SECONDS)
//    val resFuture = Await.result(collectCountInterval(rangeT), maxWaitTime)
//    resFuture.toArray.foreach(println)
  }


  @tailrec
  final def iterateFact(x: Int, result: BigInt = 1): BigInt =
    if (x == 0) result
    else iterateFact(x - 1, result * x)

  val countExpression: PartialFunction[Int, BigInt] = {
    case x: Int if (x < 0 || x > 1) => {
      if (x < 0) x
      else if (x >= 0 && x <= 1) throw new IllegalArgumentException("function not defined on [0, 1]")
      else iterateFact(x)
    }
  }

  val liftedCE: Int => Option[BigInt] = countExpression.lift

  def countInterval(range: Range, start: Int, end: Int, dst: Array[Option[BigInt]]) = {
    for (x <- start to end) dst(x) = liftedCE(range(x))
  }

  def countIntervalPar(range: Range, dst: Array[Option[BigInt]], numTasks: Int) = {
    val step = range.length / Math.min(range.length, numTasks)
    val byStep = 0 to range.length by step
    val intervals = byStep zip byStep.tail

    val tasks = for ((from, to) <- intervals) yield task(countInterval(range, from, to, dst))
    for (t <- tasks) t.join()
  }

  def mapCountInterval(src: ParSet[Int]) = {
    val result = new ConcurrentSkipListSet[BigInt]()
    for (x <- src) if (countExpression.isDefinedAt(x)) {
      result.add(countExpression(x))
    }
    result
  }

  def collectCountInterval(range: Range): Future[IndexedSeq[BigInt]] = {
    Future(range collect countExpression)
  }
}
