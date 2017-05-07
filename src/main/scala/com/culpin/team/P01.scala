package com.culpin.team

import scala.annotation.tailrec

object P01 {

  def lastBuiltIn[T](list: List[T]): T = list.last

  @tailrec
  def lastRecursive[T](list: List[T]): T = {
    list match {
      case Nil => throw new NoSuchElementException
      case head :: Nil => head
      case head :: tail => lastRecursive(tail)
    }
  }

}
