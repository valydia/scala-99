package com.culpin.team

import scala.annotation.tailrec

object P08 {

  def compressRec[T](list: List[T]): List[T] = {
    @tailrec
    def loop(res: List[T], l: List[T]): List[T] = l match {
      case Nil => res.reverse
      case head :: tail => loop(head :: res, tail.dropWhile(_ == head))
    }
    loop(Nil, list)
  }

  def compressFoldLeft[T](list: List[T]): List[T] = {
    list.foldLeft((List.empty[T], Option.empty[T])) {
      case ((acc, maybeHead), elem) =>
        maybeHead match {
          case Some(head) if head == elem => (acc, maybeHead)
          case _ => (elem :: acc, Some(elem))
        }
    }._1.reverse
  }

}
