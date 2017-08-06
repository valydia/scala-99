package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class P17Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[(Int, List[T]) => (List[T], List[T])] =
    List(P17.splitBuiltIn, P17.splitRec)


  def expectedAndInitialListGen[T](g: Gen[T]): Gen[(Int, List[T], List[T], List[T])] = {
    for {
      list1 <- Gen.listOf(g)
      list2 <- Gen.listOf(g)
    } yield {
      (list1.length,list1 ++ list2, list1, list2 )
    }
  }

  //TODO: edge case n == 0
  "split" should "split a list into two parts" in {
    check(
        forAll(expectedAndInitialListGen(arbitrary[Int])) {
          case (n, initialList, expected1, expected2) =>
            implementations[Int].forall { split =>
              split(n, initialList) == (expected1, expected2)
            }
        }
    )
  }

}
