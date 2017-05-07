package com.culpin.team

import org.scalacheck.Prop
import org.scalacheck.Arbitrary._
import org.scalatest.{ Matchers, FlatSpec }

class P01Spec extends FlatSpec with Matchers {

  def implementations[T]: List[List[T] => T] =
    List(P01.lastBuiltIn, P01.lastRecursive)


  "Last" should "throw a NoSuchElementException for empty list" in {
    implementations[Int].foreach { last =>
      intercept[NoSuchElementException] {
        last(Nil)
      }
    }
  }

  "Last" should "return the last element of a non-empty List" in {
    Prop.forAll { (l: List[Int], e: Int) =>
      val list = l :+ e
      implementations[Int].forall { last =>
        last(list) == e
      }
    }
  }

}
