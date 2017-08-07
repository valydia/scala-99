package com.culpin.team

import com.culpin.team.generator.ListGenerator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

class P19Spec extends FlatSpec with Checkers with Matchers with ListGenerator {

  def implementations[T]: List[(Int, List[T]) => List[T]] =
    List(P19.rotate, P19.rotate2)


  def expectedAndInitialListGen[T](g: Gen[T]): Gen[(Int, Int, List[T], List[T])] = {
    for {
      list1 <- Gen.listOf(g)
      list2 <- Gen.listOf(g)
      list3 <- Gen.listOf(g)
    } yield {
      (list1.length, list1.length + list2.length, list1 ++ list2 ++ list3, list2 )
    }
  }

  "Rotate" should "should leave an empty list empty" in {
    check(
      forAll(arbitrary[Int]) { n =>
        implementations[Int].forall { rotate =>
            rotate(n, Nil) == Nil
        }
      }
    )
  }


  it should "round trip" in {
    check(
        forAll(Gen.nonEmptyListOf(arbitrary[Int])) { l: List[Int] =>
          forAll(Gen.oneOf(l.indices)) { n =>
              implementations[Int].forall { rotate =>
                val r = rotate(n, l)
                rotate(-n, r) == l
              }
          }
        }
    )
  }

}
