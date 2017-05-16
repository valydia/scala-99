package com.culpin.team

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class P04Spec extends FlatSpec with Checkers {

  def implementations[T]: List[List[T] => Int] =
    List(P04.lengthBuiltIn, P04.lenghtRec, P04.lengthFoldLeft)

  "Length" should "return list length" in {
    check(
      forAll(Gen.posNum[Int]) { k: Int =>
        forAll(Gen.listOfN(k, arbitrary[Int])) { list: List[Int] =>
          implementations[Int].forall { length =>
            length(list) == k
          }
        }
      }
    )
  }

}
