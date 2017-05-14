package com.culpin.team

import scala.annotation.tailrec

object P04 {

  def lengthBuiltIn[T](list: List[T]): Int = {
    list.length
  }

  def lenghtRec[T](list: List[T]): Int = {
    @tailrec
    def loop(acc: Int, l: List[T]): Int = l match {
      case Nil => acc
      case head :: tail => loop(acc + 1, tail)
    }
    loop(0, list)
  }

  def lengthFoldLeft[T](list: List[T]): Int = {
    list.foldLeft(0) { case(acc, _) =>
      acc + 1
    }
  }

}
