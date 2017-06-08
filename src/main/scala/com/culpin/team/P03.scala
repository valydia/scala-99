package com.culpin.team

import scala.annotation.tailrec

object P03 {

  def nthBuiltIn[T](k: Int, list: List[T]): T = {
    list(k)
  }

  @tailrec
  def nthRec[T](k: Int, list: List[T]): T = {
    if (list.isEmpty || k < 0)
      throw new IndexOutOfBoundsException(k.toString)
    else {
      (k, list) match {
        case (_, Nil) => throw new IndexOutOfBoundsException(k.toString)
        case (0, h :: tail) => h
        case (n, head :: tail) => nthRec(n - 1, tail)
      }
    }
  }

}
