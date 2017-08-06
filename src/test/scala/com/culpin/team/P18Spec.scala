package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class P18Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[(Int, Int, List[T]) => List[T]] =
    List(P18.sliceBuitIn, P18.sliceFoldLeft, P18.sliceRec)


  def expectedAndInitialListGen[T](g: Gen[T]): Gen[(Int, Int, List[T], List[T])] = {
    for {
      list1 <- Gen.listOf(g)
      list2 <- Gen.listOf(g)
      list3 <- Gen.listOf(g)
    } yield {
      (list1.length, list1.length + list2.length, list1 ++ list2 ++ list3, list2 )
    }
  }


  "slice" should "extract a slice from a list." in {
    check(
        forAll(expectedAndInitialListGen(arbitrary[Int])) {
          case (i, k, initialList, expected) =>
            implementations[Int].forall { slice =>
              slice(i, k, initialList) == expected
            }
        }
    )
  }

}
