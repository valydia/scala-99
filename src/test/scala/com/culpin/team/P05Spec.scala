package com.culpin.team

import org.scalacheck.Prop.{ forAll, _ }
import org.scalatest.{ Matchers, FlatSpec }
import org.scalatest.prop.Checkers

class P05Spec extends FlatSpec with Checkers with Matchers {

  def implementations[T]: List[List[T] => List[T]] =
    List(P05.reverseBuiltIn, P05.reverseRec, P05.reverseFoldLeft)

  val properties = List(
    List.empty[String] -> List.empty[String],
    List(1, 2, 3, 4, 5) -> List(5, 4, 3, 2, 1),
    List("a", "b", "c") -> List("c", "b", "a")
  )

  "Reverse" should "reverse list" in {
    properties.foreach {
      case (list, expected) =>
        implementations[Any].foreach { reverse =>
          reverse(list) shouldBe expected
        }
    }
  }

  "Reverse" should "round trip" in {
    check(
      forAll { list: List[Int] =>
        implementations[Int].forall { reverse =>
          reverse(reverse(list)) == list
        }
      }
    )
  }

}
