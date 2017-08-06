package com.culpin.team

import scala.annotation.tailrec

object P15 {

  def duplicateN[T](n: Int, list: List[T]): List[T] =
    list.flatMap { elem => List.fill(n)(elem) }

  def duplicateNRec[T](n: Int, list: List[T]): List[T] = {

    @tailrec
    def loop(acc: List[T], l: List[T]): List[T] = l match {
      case Nil => acc.reverse
      case head :: tail => loop(List.fill(n)(head) ++ acc, tail)
    }

    loop(Nil, list)
  }

}
