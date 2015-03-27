package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.InlineParsers
import org.scalatest._

class InlineParsersSpec extends FlatSpec with Matchers {
  val parsers = InlineParsers

  "standard_text" should "match a regular sentence" in {
    val input = "some regular old input"
    val result = parsers.parseAll(parsers.standard_text, input).get

    assert(result.text == input)
  }

  it should "not match an empty string" in {
    val result = parsers.parseAll(parsers.standard_text, "")

    assert(result.isEmpty)
  }

  it should "not match a string with a special char at the front" in {
    val base_input = "something something something"
    for (c <- parsers.special_chars) {
      val result = parsers.parseAll(parsers.standard_text, c + base_input)

      assert(result.isEmpty)
    }
  }

  it should "only match up to a special char" in {
    val left_input = "the left side"
    val right_input = "the right side"

    for (c <- parsers.special_chars) {
      val result = parsers.parse(parsers.standard_text, left_input + c + right_input)

      assert(!result.isEmpty)
      assert(result.get.text == left_input)
    }
  }

}
