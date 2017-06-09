package com.culpin.team.generator

import org.scalacheck.Gen

trait ListGenerator {

  def monoElementListGen[T](g: Gen[T], minSize: Int = 1, maxSize: Int = 1000): Gen[(T, List[T])] = {
    for {
      size <- Gen.choose(minSize, maxSize)
      t <- g
    } yield t -> List.fill(size)(t)
  }

}
