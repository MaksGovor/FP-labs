package recfun

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends AnyFunSuite {
  import Main.pascal
    test("pascal: col=0,row=2") {
      assert(pascal(0,2) === 1)
  }

    test("pascal: col=1,row=2") {
      assert(pascal(1,2) === 2)
  }

    test("pascal: col=1,row=3") {
      assert(pascal(1,3) === 3)
  }

  test("pascal: col=5,row=1") {
    assertThrows[IllegalArgumentException] {
      pascal(5, 1)
    }
  }

  test("pascal: col=-1,row=1") {
    assertThrows[IllegalArgumentException] {
      pascal(-1, 1)
    }
  }
}
