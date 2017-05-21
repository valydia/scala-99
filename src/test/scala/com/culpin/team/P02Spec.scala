package com.culpin.team

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec }
import org.scalacheck.Prop._

class P02Spec extends FlatSpec with Checkers {

  val tinyListGen =
    for {
      size <- Gen.choose(0, 1)
      list <- Gen.listOfN(size, arbitrary[Int])
    } yield list

  def implementations[T]: List[List[T] => T] =
    List(P02.penultimateBuiltIn, P02.penultimate)

  "Penultimate" should "throw a NoSuchElementException for a list with a size <= 1" in {
    check(
      forAll(tinyListGen) { tinyList: List[Int] =>
        implementations[Int].forall { penultimate =>
          throws(classOf[NoSuchElementException]) {
            penultimate(tinyList)
          }
        }
      }
    )
  }

  it should "return the penultimate element of a list longer than 2" in {
    check {
      forAll { (l: List[Int], e: Int, f: Int) =>
        val nonEmptyList = l ++ List(e, f)
        implementations[Int].forall { penultimate =>
          penultimate(nonEmptyList) == e
        }
      }
    }
  }

}
