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

  def heapEquals(h1: H, h2: H): Boolean = (h1, h2) match {
    case (Nil, Nil) => true
    case _ if (isEmpty(h1) || isEmpty(h2)) =>  false
    case (hs1, hs2) => {
      findMin(hs1) == findMin(hs2) && heapEquals(deleteMin(hs1), deleteMin(hs2))
    }
  }

  def checkElem(h: H, el: Int): Boolean = h match {
    case Nil => false
    case hs1 => findMin(hs1) == el || checkElem(deleteMin(hs1), el)
  }

  def getSorted(h: H): H = {
    def getSortedAcc(h: H, acc: H): H = h match {
      case Nil => acc
      case hs => getSortedAcc(deleteMin(hs), insert(findMin(hs), acc))
    }
    getSortedAcc(h, empty)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("order of finding the minimal is correct") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    val min1 = findMin(heap)
    val min2 = findMin(deleteMin(heap))
    min1 == ord.min(x, y) && min2 == ord.max(x, y)
  }

  property("minimum element of two heaps") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    val actual = findMin(heap)
    actual == ord.min(x, y)
  }

  property("inserting one item into an empty heap and removing it") = forAll { (x: Int) =>
    val heap = insert(x, empty)
    val emptyHeap = deleteMin(heap)
    isEmpty(emptyHeap)
  }

  property("any heap is properly sorted") = forAll { (h: H) =>
    heapEquals(h, getSorted(h))
  }

  property("any merging of heaps is discouraged") = forAll { (h1: H, h2: H) =>
    val heap = meld(h1, h2)
    heapEquals(heap, getSorted(heap))
  }

  property("any heap is sorted if you find/remove the minimum") = forAll { (h: H) =>
    !isEmpty(h) ==> {
      val heap = deleteMin(h)
      heapEquals(heap, getSorted(heap))
    }
  }

  property("minimum of the meld of two heaps - minimum of one of the shining") = forAll { (h1: H, h2: H) =>
    !(isEmpty(h1) && isEmpty(h2)) ==> {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val heap = meld(h1, h2)
      findMin(heap) == ord.min(min1, min2)
    }
  }

  property("inserting two item into an empty heap and removing it") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    val emptyHeap = deleteMin(deleteMin(heap))
    isEmpty(emptyHeap)
  }

  property("adding an element larger than the minimum to the pile") = forAll{ (h: H, x: Int) => {
    !isEmpty(h) && x < findMin(h) ==> {
      val heap = insert(x, h)
      findMin(heap) == x
    }
  }}

  property("merging two heaps and comparing the same without the minimum with insertion") = forAll { (h1: H, h2: H) =>
    !isEmpty(h1) ==> {
      val minH1 = findMin(h1)
      val h1Mod = deleteMin(h1)
      val h2Mod = insert(minH1, h2)
      heapEquals(meld(h1, h2), meld(h1Mod, h2Mod))
    }
  }

  property("order of the meld arguments does not matter") = forAll { (h1: H, h2: H) =>
    heapEquals(meld(h1, h2), meld(h2, h1))
  }

  property("inserted element is in the heap") = forAll { (h: H, x: Int) =>
    !isEmpty(h) && x < findMin(h) ==> {
      val heap = insert(x, h)
      checkElem(heap, x)
    }
  }
}
