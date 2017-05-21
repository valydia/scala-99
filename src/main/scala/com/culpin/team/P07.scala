package com.culpin.team

object P07 {

  def flattenRec(list: List[Any]): List[Any] = {
    list.flatMap { elem =>
      elem match {
        case l: List[_] => flattenRec(l)
        case _ => List(elem)
      }
    }
  }

}
