package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }

class P13Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[List[T] => List[(Int, T)]] =
    List(P13.encodeDirectFoldLeft, P13.encodeDirectRec)

  def repeatedListGen[T](g: Gen[T]): Gen[(List[(Int, T)], List[T])] = {
    for {
      size <- Gen.choose(1, 1000)
      monoListList <- Gen.listOfN(size, monoElementListGen(g))
    } yield {
      monoListList.foldLeft((List.empty[(Int, T)], List.empty[T])) {
        case ((expectedList, repeatedList), (elem, monoList)) =>
          if (expectedList.isEmpty || elem != expectedList.head._2) {
            ((monoList.length, elem) :: expectedList, monoList ++ repeatedList)
          } else {
            (expectedList, repeatedList)
          }
      }
    }
  }

  "encode" should "run-length encoding of a list" in {
    check(
      forAll(repeatedListGen(arbitrary[Int])) {
        case (expectedList, repeatedList) =>
          implementations[Int].forall { encode =>
            val actualList = encode(repeatedList)
            actualList == expectedList
          }
      }
    )
  }

}
