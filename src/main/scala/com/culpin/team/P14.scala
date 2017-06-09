package com.culpin.team

object P14 {

  def duplicate[T](list: List[T]): List[T] =
    list.flatMap { elem => List(elem, elem) }

}
