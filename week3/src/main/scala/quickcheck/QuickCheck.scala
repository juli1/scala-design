package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  val genEmptyHeap: Gen[H] = Gen.const(empty) // helper function

  /*
   * We give either empty or non-empty.
   */
  lazy val genHeap: Gen[H] = oneOf(
    genEmptyHeap,
    for {
      x <- Arbitrary.arbitrary[Int]
      h <- Gen.oneOf(genEmptyHeap, genHeap)
    } yield insert(x, h)

  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap
    * should get the smallest of the two elements back.
    */
  property("prop1") = forAll { (a: Int, b : Int) =>
    val h = insert(a, insert(b, empty))
    val min = findMin(h)
    if (a < b) {
      min == a
    }
    else {
      min == b
    }
  }

  /**
   * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   */
  property("prop2") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("prop3-1") = forAll { (a: Int, b: Int) =>
    {
      def toList(h: H): List[A] = {
        if (isEmpty(h)) List()
        else findMin(h) :: toList(deleteMin(h))
      }
      var h : H = empty
      var correctList : List[Int] = List()
      val rand = new scala.util.Random
      for ( i <- 1 to 100) {
        val newNumber = rand.nextInt()
        h = insert(newNumber, h)
      }
      val list = toList(h)
      list.sorted == list

      var number1 : Int = findMin(h)
      while (! isEmpty(h))
        {
          h = deleteMin(h)
          if (! isEmpty (h)) {

            val number2: Int = findMin(h)
            //print("n1="+ number1 + ";n2=" + number2)
            assert( number1 <= number2)
            number1 = number2

          }
        }
      list.sorted == list
    }
  }
  property("prop3-2") = forAll { (h: H) =>
    def toList(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: toList(deleteMin(h))
    }
    val h1 = insert(1, insert(2, insert(10, insert(3, insert(5, insert(4, empty))))))
    val h2 = insert(7, insert(9, insert(8, insert(6, empty))))
    val melded = meld(h1, h2)
    val l = toList(melded)
    //println(l)
    l == List(1,2,3,4,5,6,7,8,9,10)
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("prop4") = forAll { (a: Int, b: Int) =>
    val firstheap = insert(a, empty)
    val secondheap = insert(b, empty)

    val merged = meld(firstheap,secondheap)
    val min =findMin(merged)
    if (a < b) {
      min == a
    }
    else {
      min == b
    }
  }
}
