package recfun

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class calcExpressionSuite extends AnyFunSuite {
  import Main.calcExpression

  test("calcExpression: x = 3") {
    assert(calcExpression(3) === 6)
  }

  test("calcExpression: x = 2") {
    assert(calcExpression(2) === 2)
  }

  test("calcExpression: x = -1") {
    assert(calcExpression(-1) === 0)
  }

  test("calcExpression: x = 0") {
    assertThrows[IllegalArgumentException] {
      calcExpression(0)
    }
  }

  test("calcExpression: x = 1") {
    assertThrows[IllegalArgumentException] {
      calcExpression(1)
    }
  }
}
