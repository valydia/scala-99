package com.culpin.team.generator

import org.scalacheck.Gen
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{ forAll, _ }

trait ListGenerator {

  def monoElementListGen[T](g: Gen[T], minSize: Int = 1, maxSize: Int = 1000): Gen[(T, List[T])] = {
    for {
      size <- Gen.choose(minSize, maxSize)
      t <- g
    } yield t -> List.fill(size)(t)
  }

}

class ListGeneratorSpec extends FlatSpec with Checkers with Matchers with ListGenerator {

  "monoElementListGen" should "generate list with same element" in {
    check(
      forAll(monoElementListGen(arbitrary[Int])) { tuple =>
        val (e, list) = tuple
        val set = list.toSet
        set.size == 1 &&  set.head == e
      }
    )
  }
}
