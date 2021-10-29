package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(x, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum element of two heaps") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    val actual = findMin(heap)
    if (x > y) actual == y else actual == x
  }
}
