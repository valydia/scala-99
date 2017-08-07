package com.culpin.team

object P19 {

  def rotate[T](n: Int, list: List[T]): List[T] = {
    val (beginning, end) =
      if (n >= 0)
        list.splitAt(n)
      else
        list.splitAt(list.length + n)
    end ++ beginning
  }

  def rotate2[T](n: Int, list: List[T]): List[T] = {
    if (n >= 0)
      list.drop(n) ++ list.take(n)
    else
      list.takeRight(-n) ++ list.dropRight(-n)
  }

}
