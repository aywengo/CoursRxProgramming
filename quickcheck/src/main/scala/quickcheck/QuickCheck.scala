package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, empty), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("hint1") = forAll { (n1: Int, n2: Int) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("hint2") = forAll { (n: Int) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("hint3") = forAll { (h: H) =>
    def sorted(h : H) : Boolean =
      if (isEmpty(h))
        true
      else {
        val head = findMin(h)
        val rest = deleteMin(h)

        isEmpty(rest) || (head <= findMin(rest)) && sorted(rest)
      }

    sorted(h)
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1,h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      isEmpty(h1) && isEmpty(h2) || (
        (findMin(h1) == findMin(h2)) && heapEqual(deleteMin(h1), deleteMin(h2))
        )

    heapEqual(
      meld(h1, h2),
      meld(
        deleteMin(h1),
        insert(findMin(h1), h2)
      )
    )
  }

}
