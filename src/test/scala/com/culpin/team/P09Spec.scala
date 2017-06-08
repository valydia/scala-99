package com.culpin.team

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }

class P09Spec extends FlatSpec with Checkers with Matchers {

  def implementations[T]: List[List[T] => List[List[T]]] =
    List(P09.packRec, P09.packFoldLeft)

  def monoElementListGen[T](g: Gen[T]): Gen[(T, List[T])] = {
    for {
      size <- Gen.choose(1, 1000)
      t <- g
    } yield (t, List.fill(size)(t))
  }

  def repeatedListGen[T](g: Gen[T]): Gen[(List[List[T]], List[T])] = {
    for {
      size <- Gen.choose(1, 1000)
      monoListList <- Gen.listOfN(size, monoElementListGen(g))
    } yield {
      monoListList.foldLeft((List.empty[List[T]], List.empty[T])) {
        case ((expectedList, repeatedList), (elem, monoList)) =>
          if (expectedList.isEmpty || elem != expectedList.head.head) {
            (monoList :: expectedList, monoList ++ repeatedList)
          } else {
            (expectedList, repeatedList)
          }
      }
    }
  }

  "pack" should "pack consecutive duplicates of list elements into sublists" in {
    check(
      forAll(repeatedListGen(arbitrary[Int])) {
        case (expectedList, repeatedList) =>
          implementations[Int].forall { pack =>
            val actualList = pack(repeatedList)
            actualList == expectedList
          }
      }
    )
  }

}
