package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.math.max

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  def minimum(h: H): Int = {
    if (isEmpty(h)) 0 else findMin(h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

    property("gen1") = forAll { (h: H) =>
      val m = minimum(h)
      findMin(insert(m, h)) == m
    }

    property("w") = forAll { (first: H, second: H) =>
      val firstMin = minimum(first)
      val secondMin = minimum(second)
      val min = findMin(meld(first, second))
      if (firstMin >= secondMin) min == secondMin else  min == firstMin
    }

    property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = {
      isEmpty(deleteMin(insert(0, empty)))
    }

    property("If you insert any two elements into an empty heap," +
      "finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll { (a: Int, b: Int, c: Int) =>
      findMin(deleteMin(deleteMin(insert(a, insert(b, insert(c, empty)))))) == max(a, max(b, c))
    }
  //
  property("should properly delete from melded") = {
    findMin(deleteMin(meld(insert(-10, empty), insert(-20, empty)))) == -10
  }


  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") = forAll { (h: H) =>
    def minSequenceAppender(t: H, list: List[Int]): List[Int] = {
      if (!isEmpty(t)) {
        minSequenceAppender(deleteMin(t), minimum(t) :: list)
      } else list.reverse
    }

    val list = minSequenceAppender(h, Nil)
    list == list.sorted
  }



//  property
}
