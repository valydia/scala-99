package com.culpin.team

import scala.annotation.tailrec

object P17 {

  def splitBuiltIn[T](n: Int, list: List[T]): (List[T], List[T]) =
    list.splitAt(n)

  def splitRec[T](n: Int, list: List[T]): (List[T], List[T]) = {
    @tailrec
    def loop(k: Int, acc: List[T], l: List[T]): (List[T], List[T]) = {
      (k, l) match {
        case (0, _) => (acc.reverse, l)
        case (_, Nil) => (acc.reverse, l)
        case (_, head :: tail) => loop(k - 1, head :: acc, tail)
      }
    }
    loop(n, Nil, list)
  }

}
