package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class P15Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[(Int,List[T]) => List[T]] =
    List(P15.duplicateN)

  def repeatedListGen[T](g: Gen[T], monolistSize: Int): Gen[(List[T], List[T])] = {
    for {
      size <- Gen.choose(1, 1000)
      monoListList <- Gen.listOfN(size, monoElementListGen(g, monolistSize, monolistSize))
    } yield {
      monoListList.foldLeft((List.empty[T], List.empty[T])) {
        case ((expectedList, repeatedList), (elem, monoList)) =>
           (monoList ++ expectedList, elem :: repeatedList)
      }
    }
  }

  "duplicateN" should "duplicate the elements of a list a given number of times." in {
    check(
      forAll(Gen.choose(1, 1000)) { n =>
        forAll(repeatedListGen(arbitrary[Int], n)) {
          case (expectedList, repeatedList) =>
            implementations[Int].forall { duplicateN =>
              duplicateN(n, repeatedList) == expectedList
            }
        }
      }

    )
  }

}
