package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone._
import org.scalatest._

class InlineParsersSpec extends FlatSpec {
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

  "emphasized_text" should "match characters between /" in {
    val input = "/hello/"
    val result = parsers.parseAll(parsers.emphasized_text, input).get

    assert(result.text == "hello")
  }

  "weighted_text" should "match characters between *" in {
    val input = "*hello*"
    val result = parsers.parseAll(parsers.weighted_text, input).get

    assert(result.text == "hello")
  }

  "underlined_text" should "match characters between _" in {
    val input = "_hello_"
    val result = parsers.parseAll(parsers.underlined_text, input).get

    assert(result.text == "hello")
  }

  "struckthrough_text" should "match characters between ~" in {
    val input = "~hello~"
    val result = parsers.parseAll(parsers.struckthrough_text, input).get

    assert(result.text == "hello")
  }

  "monospaced_text" should "match characters between `" in {
    val input = "`hello`"
    val result = parsers.parseAll(parsers.monospaced_text, input).get

    assert(result.text == "hello")
  }

  "link_text" should "match text inside []" in {
    val input = "[hello]"
    val result = parsers.parseAll(parsers.link_text, input).get

    assert(result.children.length == 1)
    assert(result.children(0).asInstanceOf[StandardText].text == "hello")
  }

  it should "recognize a link's target, after a link, inside ()" in {
    val input = "[hello](goodbye)"
    val result = parsers.parseAll(parsers.link_text, input).get

    assert(result.to.target == "goodbye")
  }

  it should "recursively parse inline nodes" in {
    val input = "[_hello_/this/*is*something~bold~]"
    val result = parsers.parseAll(parsers.link_text, input).get

    assert(result.children.length == 5)
  }

  it should "not parse elements of the target as InlineNodes" in {
    val input = "[hello](_hello_/this/*is*something~bold~)"
    val result = parsers.parseAll(parsers.link_text, input).get

    assert(result.to.target == "_hello_/this/*is*something~bold~")
    assert(result.to.children.isEmpty)
  }

  it should "not allow nested links" in {
    val input = "[[hello]]"
    val result = parsers.parseAll(parsers.link_text, input)

    assert(result.isEmpty)
  }

  "footnote_text" should "match text between {}" in {
    val input = "{hello}"
    val result = parsers.parseAll(parsers.footnote_text, input).get

    assert(result.children.length == 1)
    assert(result.children(0).asInstanceOf[StandardText].text == "hello")
  }

  it should "recursively parse inline nodes" in {
    val input = "{_hello_/this/*is*something~bold~}"
    val result = parsers.parseAll(parsers.footnote_text, input).get

    assert(result.children.length == 5)
  }

  it should "not allow nested footnotes" in {
    val input = "{{hello}}"
    val result = parsers.parseAll(parsers.footnote_text, input)

    assert(result.isEmpty)
  }

  "element" should "prevent nesting" in {
  }
}
