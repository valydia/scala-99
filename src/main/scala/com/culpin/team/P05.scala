package com.culpin.team

import scala.annotation.tailrec

object P05 {

  def reverseBuiltIn[T](list: List[T]): List[T] = {
    list.reverse
  }

  def reverseRec[T](list: List[T]): List[T] = {
    @tailrec
    def loop(acc: List[T], l: List[T]): List[T] = l match {
      case Nil => acc
      case head :: tail => loop(head :: acc, tail)
    }
    loop(Nil, list)
  }

  def reverseFoldLeft[T](list: List[T]): List[T] = {
    list.foldLeft(List.empty[T]) {
      case (acc, elem) =>
        elem :: acc
    }
  }

}
