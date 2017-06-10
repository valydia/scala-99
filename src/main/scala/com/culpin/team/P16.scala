package com.culpin.team

import scala.annotation.tailrec

object P16 {

  def drop[T](n: Int, list: List[T]): List[T] =
    list.zipWithIndex.filterNot { case (elem, index) => (index + 1) % n == 0 }.map(_._1)

  def dropRec[T](n: Int, list: List[T]): List[T] = {
    @tailrec
    def loop(index: Int, acc: List[T], l: List[T]): List[T] = {
      (l, index) match {
        case (Nil, _) => acc.reverse
        case (head :: tail, i) if (i + 1) % n == 0 => loop(index + 1, acc, tail)
        case (head :: tail, i) => loop(index + 1, head ::acc, tail)
      }
    }
    loop(0, Nil, list)
  }

  def dropFoldLeft[T](n: Int, list: List[T]): List[T] = {
    val (result, _) =
    list.foldLeft((List.empty[T], 0)) {
      case ((acc, index), elem) =>
        if ((index + 1) % n == 0)
          (acc, index + 1)
        else
          (elem :: acc, index + 1)
    }
    result.reverse
  }


}
