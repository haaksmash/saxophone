/*
 * saxophone - a markup processing program
 * Copyright (C) 2015  Haak Saxberg
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.primitives.{RawText, StandardText}
import org.scalatest._

class InlineParsersSpec extends FlatSpec {
  val parsers = InlineParsers

  "standardText" should "match a regular sentence" in {
    val input = "some regular old input"
    val result = parsers.parseAll(parsers.standardText(Set()), input).get

    assert(result.text == input)
  }

  it should "not match an empty string" in {
    val result = parsers.parseAll(parsers.standardText(Set()), "")

    assert(result.isEmpty)
  }

  it should "not match a string with a special char at the front" in {
    val base_input = "something something something"
    for (c <- parsers.special_char_to_tracking_and_ending_char) {
      val result = parsers.parseAll(parsers.standardText(Set(c._1)), c + base_input)

      assert(result.isEmpty)
    }
  }

  it should "only match up to a special char" in {
    val left_input = "the left side"
    val right_input = "the right side"

    for (c <- parsers.special_char_to_tracking_and_ending_char.keys) {
      val result = parsers.parse(parsers.standardText(Set(c)), left_input + c + right_input)

      assert(!result.isEmpty)
      assert(result.get.text == left_input)
    }
  }

  it should "allow us to escape special chars" in {
    val input = "i want to be a \\*"

    val result = parsers.parse(parsers.standardText(Set()), input).get

    assert(result == StandardText("i want to be a *"))
  }

  it should "allow escaped `" in {
    val input = "\\`hello\\`"
    val result = parsers.parseAll(parsers.standardText(Set()), input).get

    assert(result.text == "`hello`")
  }

  "raw_text" should "match characters between |" in {
    val input = "|`*RAW*`|"
    val result = parsers.parseAll(parsers.raw_text, input).get

    assert(result.text == "`*RAW*`")
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

  it should "not care about other special chars inside it" in {
    val input = "`**hello**`"
    val result = parsers.parseAll(parsers.monospaced_text, input).get

    assert(result.text == "**hello**")
  }


  "link_text" should "match text inside []" in {
    val input = "[hello]"
    val result = parsers.parseAll(parsers.link_text, input).get

    assert(result.children.length == 1)
    assert(result.children.head.asInstanceOf[StandardText].text == "hello")
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
    assert(result.children.head.asInstanceOf[StandardText].text == "hello")
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

  "element" should "successfully parse things" in {
    for ((s,(_,e)) <- parsers.special_char_to_tracking_and_ending_char) {
      val result = parsers.parse(parsers.element(Set.empty), s"${s}hello$e")

      assert(!result.isEmpty)
    }

  }

  it should "prevent nesting" in {
    for ((s, (f:String, e)) <- parsers.special_char_to_tracking_and_ending_char) {
      val nested_result = parsers.parseAll(parsers.element(visited = Set(f)), s"${s}hello$e")

      assert(nested_result.isEmpty)
    }
  }

  "elements" should "recognize raw marker strings" in {
    val raw_result = parsers.parseAll(parsers.elements(), "and |this is **RAW**|")
    assert(!raw_result.isEmpty)
    assert(raw_result.get == Seq(StandardText("and "), RawText("this is **RAW**")))
  }

}
