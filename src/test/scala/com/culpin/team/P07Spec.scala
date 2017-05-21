package com.culpin.team

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }

class P07Spec extends FlatSpec with Checkers with Matchers {

  val implementations: List[List[Any] => List[Any]] =
    List(P07.flattenRec)

  def nestedList2Gen[T](g: Gen[T]) = Gen.listOf(Gen.oneOf(g, Gen.listOf(g)))

  def nestedList3Gen[T](g: Gen[T]) = Gen.listOf(Gen.oneOf(g, nestedList2Gen(g)))

  "Flatten" should "flatten a nested list structure" in {
    val list: List[Any] = List(List(1, 1), 2, List(3, List(5, 8)))
    implementations.foreach { flatten =>
      flatten(list) shouldBe List(1, 1, 2, 3, 5, 8)
    }
  }

  it should "should be idempotent" in {
    check(
      forAll(nestedList3Gen[Int](arbitrary[Int])) { nestedList: List[Any] =>
        implementations.forall { flatten =>
          val flat = flatten(nestedList)
          flatten(flat) == flat
        }
      }
    )
  }

}
