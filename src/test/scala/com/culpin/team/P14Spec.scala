package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class P14Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[List[T] => List[T]] =
    List(P14.duplicate)

  def repeatedListGen[T](g: Gen[T]): Gen[(List[T], List[T])] = {
    for {
      size <- Gen.choose(1, 1000)
      monoListList <- Gen.listOfN(size, monoElementListGen(g, 2, 2))
    } yield {
      monoListList.foldLeft((List.empty[T], List.empty[T])) {
        case ((expectedList, repeatedList), (elem, monoList)) =>
           (monoList ++ expectedList, elem :: repeatedList)
      }
    }
  }

  "duplicate" should "duplicate the elements of a list" in {
    check(
      forAll(repeatedListGen(arbitrary[Int])) {
        case (expectedList, repeatedList) =>
          implementations[Int].forall { duplicate =>
              duplicate(repeatedList) == expectedList
          }
      }
    )
  }

}
