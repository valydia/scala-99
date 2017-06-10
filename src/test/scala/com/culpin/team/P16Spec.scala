package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Gen.listOfN

class P16Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[(Int, List[T]) => List[T]] =
    List(P16.drop, P16.dropRec, P16.dropFoldLeft)


  def listOfSizeNandNPlus1Gen[T](g: Gen[T], n: Int): Gen[(List[T], List[T])] = {
    for {
      listN <- listOfN(n - 1, g)
      e <- g
    } yield (listN, listN :+ e)

  }

  def expectedAndInitialListGen[T](g: Gen[T], n: Int): Gen[(List[T], List[T])] = {
    for {
      size <- Gen.choose(1, 1000)
      listOfSizeNandNPlus1Lists <- listOfN(size,listOfSizeNandNPlus1Gen(g, n))
      sizeSmallerThanN <- Gen.choose(0, Math.max(0, n - 1))
      listSmallerThanN <- listOfN(sizeSmallerThanN, g)
    } yield {
      val (exectedList, initialList) =
      listOfSizeNandNPlus1Lists.foldLeft((List.empty[T], List.empty[T])) {
        case ((expectedList, initialList), (listOfN,listofNPlus1)) =>
          (listOfN ++ expectedList, listofNPlus1 ++ initialList)
      }
      (exectedList ++ listSmallerThanN, initialList ++ listSmallerThanN)
    }
  }

  //TODO: edge case n == 0
  "drop" should "duplicate the elements of a list a given number of times." in {
    check(
      forAll(Gen.choose(1, 1000)) { n =>
        forAll(expectedAndInitialListGen(arbitrary[Int], n)) {
          case (expectedList, initialList) =>
              implementations[Int].forall { dropN =>
              dropN(n, initialList) == expectedList
            }
        }
      }
    )
  }

}
