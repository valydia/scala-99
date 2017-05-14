package com.culpin.team

import org.scalacheck.Prop.forAll
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }
import org.scalacheck.Prop._

class P02Spec extends FlatSpec with Matchers with Checkers {

  val tinyListGen =
    for {
      size <- Gen.choose(0, 1)
      list <- Gen.listOfN(size, Gen.chooseNum(Int.MinValue, Int.MinValue))
    } yield list

  implicit lazy val listArb: Arbitrary[List[Int]] = Arbitrary(tinyListGen)

  def implementations[T]: List[List[T] => T] =
    List(P02.penultimateBuiltIn, P02.penultimate)


  "Penultimate" should "throw a NoSuchElementException for a list with a size <= 1" in {
    check(
      forAll { tinyList: List[Int] =>
        implementations[Int].forall { penultimate =>
          throws(classOf[NoSuchElementException]){
            penultimate(tinyList)
          }
        }
      }
    )
  }

  "Penultimate" should "return the penultamate element of a non-empty List" in {
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
