package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }

class P12Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[List[(Int, T)] => List[T]] =
    List(P12.decode)

  def repeatedListGen[T](g: Gen[T]): Gen[(List[T], List[(Int, T)])] = {
    for {
      size <- Gen.choose(1, 1000)
      monoListList <- Gen.listOfN(size, monoElementListGen(g))
    } yield {
      monoListList.foldLeft((List.empty[T], List.empty[(Int, T)])) {
        case ((expectedList, repeatedList), (elem, monoList)) =>
          if (repeatedList.isEmpty || elem != repeatedList.head._2) {
            (monoList ++ expectedList, (monoList.length, elem) :: repeatedList)
          } else {
            (expectedList, repeatedList)
          }
      }
    }
  }

  "decode" should "decode a run-length encoded list." in {
    check(
      forAll(repeatedListGen(arbitrary[Int])) {
        case (expectedList, repeatedList) =>
          implementations[Int].forall { decode =>
            val actualList = decode(repeatedList)
            actualList == expectedList
          }
      }
    )
  }

}
