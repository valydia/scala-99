package com.culpin.team

import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

class P01Spec extends FlatSpec with Checkers {

  def implementations[T]: List[List[T] => T] =
    List(P01.lastBuiltIn, P01.lastRecursive)

  "Last" should "throw a NoSuchElementException for empty list" in {
    implementations[Int].foreach { last =>
      intercept[NoSuchElementException] {
        last(Nil)
      }
    }
  }

  it should "return the last element of a non-empty List" in {
    check {
      forAll { (l: List[Int], e: Int) =>
        val nonEmptyList = l :+ e
        implementations[Int].forall { last =>
          last(nonEmptyList) == e
        }
      }
    }
  }

}
