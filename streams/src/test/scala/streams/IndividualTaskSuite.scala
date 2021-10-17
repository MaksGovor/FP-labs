package streams

import streams.IndividualTask.{countExpression, toList}

class IndividualTaskSuite extends munit.FunSuite {
  val inc: BigInt => BigInt = x => x + 1
  val deInc: BigInt => BigInt = x => x - 1
  val transform = List(inc, deInc)

  val range = (-250 to 25)
  val testList = range filter countExpression.isDefinedAt map countExpression

  test("Test map on testList") {
    val testMap = testList.map(elem => transform.foldLeft(elem)((v, f) => f(v)))
    assertEquals(testMap, testList)
  }

  test("Test takeWhile, dropWhile, span on testList") {
    val testTakeWhile = testList.takeWhile(_ <= 40320)
    val testDropWhile = testList.dropWhile(_ <= 40320)
    val testSpan = testList.span(_ <= 40320)
    val expectedTakenList = (-250 to 8).filter(countExpression.isDefinedAt).map(countExpression)
    val expectedDroppedList = (9 to 25).filter(countExpression.isDefinedAt).map(countExpression)
    assertEquals(testTakeWhile, expectedTakenList)
    assertEquals(testDropWhile, expectedDroppedList)
    assertEquals(testSpan, (expectedTakenList, expectedDroppedList))
  }

  test("Test find, count on testList") {
    assertEquals(testList.find(_ == 6), Some(BigInt(6)))
    assertEquals(testList.find(_ <= 6), Some(BigInt(0)))
    assertEquals(testList.find(_ < 0), None)
    assertEquals(testList.count(_ == 6), 1)
    assertEquals(testList.count(_ <= 6), 252)
  }

  test("Test scanLeft, scanRight on testList") {
    val toTest = testList.filter(x => x > 0 && x <= 720) // List(2, 6, 24, 120, 720)
    val expectedLeft = List(0, 2, 8, 32, 152, 872).map(BigInt(_)).toIndexedSeq
    val expectedRight = List(872, 870, 864, 840, 720, 0).map(BigInt(_)).toIndexedSeq
    assertEquals(toTest.scanLeft(BigInt(0))(_ + _), expectedLeft)
    assertEquals(toTest.scanRight(BigInt(0))(_ + _), expectedRight)
  }

  test("Test reduceLeft, reduceRight, foldLeft, foldRight on testList") {
    val toTest = testList.filter(x => x > 0 && x <= 720) // List(2, 6, 24, 120, 720)
    assertEquals(toTest.reduceLeft(_ - _), BigInt(-868))
    assertEquals(toTest.reduceRight(_ - _), BigInt(620))
    assertEquals(toTest.foldLeft(BigInt(8))(_ - _), BigInt(-864))
    assertEquals(toTest.foldRight(BigInt(720))(_ - _), BigInt(-100))
  }

  test("Test collect on testList") {
    val toTest = testList.filter(x => x > 0 && x <= 720) // List(2, 6, 24, 120, 720)
    val filterAndMap: PartialFunction[BigInt, BigInt] = {
      case x: BigInt if x > 2 && x < 100 => x + 1
    }
    val expected = List(7, 25).map(BigInt(_)).toIndexedSeq
    assertEquals(toTest collect filterAndMap, expected)
  }

  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds
}
