package com.culpin.team

object P10 {

  def encodeFoldLeft[T](list: List[T]): List[(Int, T)] = {
    P09.packFoldLeft(list).map(l => (l.length, l.head))
  }

  def encodeRec[T](list: List[T]): List[(Int, T)] = {
    P09.packRec(list).map(l => (l.length, l.head))
  }
}
