package com.culpin.team

import scala.annotation.tailrec

object P09 {

  def packRec[T](list: List[T]): List[List[T]] = {
    @tailrec
    def loop(l: List[T], res: List[List[T]]): List[List[T]] = (l, res) match {
      case (Nil, _) => res.reverse
      case (head :: tail, Nil) => loop(tail, List(head) :: Nil)
      case (h :: t, head :: tail) =>
        if (head.isEmpty || head.head != h)
          loop(t, List(h) :: res)
        else
          loop(t, (h :: head) :: tail)
    }
    loop(list, Nil)
  }

  def packFoldLeft[T](list: List[T]): List[List[T]] = {
    list.foldLeft(List.empty[List[T]]) {
      case (acc, elem) =>
        if (acc.isEmpty || acc.head.head != elem)
          List(elem) :: acc
        else
          (elem :: acc.head) :: acc.tail
    }.reverse
  }

}
