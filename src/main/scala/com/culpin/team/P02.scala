package com.culpin.team

import scala.annotation.tailrec

object P02 {

  def penultimateBuiltIn[T](list: List[T]): T = {
    val penultimateIndex = list.length - 2
    if (penultimateIndex < 0)
      throw new NoSuchElementException
    else
      list(penultimateIndex)
  }

  @tailrec
  def penultimate[T](list: List[T]): T = {
    list match {
      case h :: _ :: Nil => h
      case head :: tail => penultimate(tail)
      case _ => throw new NoSuchElementException
    }
  }

}
