package com.culpin.team

import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }
import org.scalacheck.Arbitrary.arbitrary

class P06Spec extends FlatSpec with Checkers with Matchers {

  def implementations[T]: List[List[T] => Boolean] =
    List(P06.palindromeBuiltIn, P06.palindromeRec)

  def palindromeGen[T](g: Gen[T]): Gen[List[T]] = for {
    base <- Gen.listOf(g)
    middle <- Gen.option(g)
  } yield {
    base ++ middle.toList ++ base.reverse
  }

  "Palindrome" should "find out non-palindrome" in {
    implementations[String].foreach { palindrome =>
      palindrome(List("a", "b", "c")) shouldBe false
    }
  }

  "Palindrome" should "find out whether a list is a palindrome" in {
    check(
      forAll(palindromeGen[Int](arbitrary[Int])) { list: List[Int] =>
        implementations[Int].forall { palindrome =>
          palindrome(list) == true
        }
      }
    )
  }

}
