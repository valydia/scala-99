package com.culpin.team

import org.scalacheck.Prop.{forAll, _}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class P03Spec extends FlatSpec  with Checkers {



  def implementations[T]: List[(Int,List[T]) => T] =
    List(P03.nthBuiltIn, P03.nthRec)


  def genBoundedList[T](maxSize: Int, g: Gen[T]): Gen[List[T]] = {
    Gen.choose(0, maxSize) flatMap { sz => Gen.listOfN(sz, g) }
  }

  def genListWithExpectedElem[T](k: Int, v:T,  g: Gen[T]): Gen[List[T]] = {
    for {
      beginning <- Gen.listOfN(k, g)
      end <- Gen.listOf[T](g)
    } yield beginning ++ List(v) ++ end
  }


  "Nth" should "throw a IndexOutOfBoundsException for empty list" in {
    check(
      forAll { k: Int =>
        implementations[Int].forall { nth =>
          throws(classOf[IndexOutOfBoundsException]){
            nth(k, Nil)
          }
        }
      }
    )
  }

  it should "throw a IndexOutOfBoundsException for negative index" in {
    check(
      forAll(Gen.negNum[Int]) { k: Int =>
        forAll { list: List[Int] =>
          implementations[Int].forall { nth =>
            throws(classOf[IndexOutOfBoundsException]){
              nth(k, list)
            }
          }
        }
      }
    )
  }

  it should "throw a IndexOutOfBoundsException when index bigger than the list size" in {
    check(
      forAll(Gen.posNum[Int]){ k: Int =>
        forAll(genBoundedList[Int](k - 1, arbitrary[Int])){ list: List[Int] =>
          implementations[Int].forall { nth =>
            throws(classOf[IndexOutOfBoundsException]){
              nth(k, list)
            }
          }
        }
      }
    )
  }

  it should "find the nth element of a list" in {
    check(
      forAll(Gen.posNum[Int], arbitrary[Int]){ (k: Int, v: Int) =>
        forAll(genListWithExpectedElem(k, v, arbitrary[Int])){ list: List[Int] =>
          implementations[Int].forall { nth =>
              nth(k, list) == v
          }
        }
      }
    )
  }



}
