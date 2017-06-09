package com.culpin.team

import scala.annotation.tailrec

object P13 {

  def encodeDirectFoldLeft[T](list: List[T]): List[(Int, T)] =
    list.foldLeft(List.empty[(Int, T)]) {
      case (acc, elem) =>
        if (acc.isEmpty || acc.head._2 != elem)
          (1, elem) :: acc
        else {
          val (n, e) :: tail = acc
          (n + 1, e) :: tail
        }
    }.reverse

  def encodeDirectRec[T](list: List[T]): List[(Int, T)] = {
    @tailrec
    def loop(acc: List[(Int,T)], l: List[T]): List[(Int, T)] = l match {
      case Nil => acc.reverse
      case head :: tail =>
        if(acc.isEmpty || acc.head._2 != head)
          loop((1,head) :: acc, tail)
        else {
          val (c, e) :: t = acc
          loop((c + 1, e) :: t, tail)
        }
    }
    loop(Nil, list)
  }

}
