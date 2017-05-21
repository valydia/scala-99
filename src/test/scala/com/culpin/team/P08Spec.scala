package com.culpin.team

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class P08Spec extends FlatSpec with Checkers with Matchers {

  def implementations[T]: List[List[T] => List[T]] =
    List(P08.compressRec, P08.compressFoldLeft)



  def monoElementListGen[T](g: Gen[T]): Gen[(T,List[T])] = {
    for {
      size <- Gen.choose(1,1000)
      t <- g
    } yield (t, List.fill(size)(t))
  }

  def repeatedListGen[T](g: Gen[T]): Gen[(List[T],List[T])] = {
    for {
      size <- Gen.choose(1,1000)
      monoListList <- Gen.listOfN(size, monoElementListGen(g))
    } yield {
      monoListList.foldLeft((List.empty[T],List.empty[T])){ case ((expectedList, repeatedList), (elem, monoList)) =>
        if (expectedList.isEmpty || elem != expectedList.head) {
          (elem :: expectedList, monoList ++ repeatedList)
        } else {
          (expectedList, repeatedList)
        }
      }
    }
  }

  def hasConsecutiveElem[T](list: List[T]): Boolean = {
    list.sliding(2).exists(l => l.head == l.last)
  }


  "Compress" should "eliminate consecutive duplicates of list elements." in {
    check(
      forAll(repeatedListGen(arbitrary[Int])) { case (expectedList, repeatedList)=>
        implementations[Int].forall { compress =>
          val actualList = compress(repeatedList)
          actualList == expectedList && !hasConsecutiveElem(actualList)
        }
      }
    )
  }


}
