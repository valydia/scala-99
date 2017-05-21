package com.culpin.team

import scala.annotation.tailrec

object P06 {

  def palindromeBuiltIn[T](list: List[T]): Boolean =
    list == list.reverse

  def palindromeRec[T](list: List[T]): Boolean = {
    @tailrec
    def loop(l: List[T]): Boolean = l match {
      case Nil => true
      case head :: Nil => true
      case x :: y :: Nil => x == y
      case head :: tail if head == tail.last => loop(tail.dropRight(1))
      case _ => false
    }
    loop(list)
  }

}
