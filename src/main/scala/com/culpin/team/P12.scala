package com.culpin.team

object P12 {

  def decode[T](list: List[(Int, T)]): List[T] =
    list.flatMap { case (n, e) => List.fill(n)(e) }

}
