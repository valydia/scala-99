package com.culpin.team

import scala.annotation.tailrec

object P18 {

  def sliceBuitIn[T](from: Int, until: Int, list: List[T]): List[T] =
    list.slice(from, until)

  def sliceRec[T](from: Int, until: Int, list: List[T]): List[T] = {
    @tailrec
    def loop(index: Int, acc: List[T], l: List[T]): List[T] = {
      (l, index) match {
        case (Nil, _) => acc.reverse
        case (head :: tail, i) if i < from => loop(index + 1, acc, tail)
        case (head :: tail, i) if from <= i && i < until => loop(index + 1, head :: acc, tail)
        case (head :: tail, i) if i >= until => acc.reverse
      }
    }
    loop(0, Nil, list)
  }

  def sliceFoldLeft[T](from: Int, until: Int, list: List[T]): List[T] = {
    val (result, _) =
      list.foldLeft((List.empty[T], 0)) {
        case ((acc, index), elem) =>
          if (index < from)
            (acc, index + 1)
          else if (from <= index && index < until)
            (elem :: acc, index + 1)
          else
            (acc, index + 1)
      }
    result.reverse
  }
}
