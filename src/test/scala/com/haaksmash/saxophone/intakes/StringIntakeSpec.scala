package com.haaksmash.saxophone.intakes

import org.scalatest._

class StringIntakeSpec extends FlatSpec {
  "StringIntake" should "successfully evaluate a string" in {
    val document = StringIntake("# Title\na paragraph\n\nanother paragraph /with emphasis/ and *weight*")

    assert(document.isDefined)
    assert(document.get.children.length == 3)
    assert(document.get.children.last.children.toSeq.length == 4)
  }

}
