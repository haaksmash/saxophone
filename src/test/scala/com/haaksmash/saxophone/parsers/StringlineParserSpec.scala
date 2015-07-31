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

import org.scalatest._


class StringLineParserSpec extends FlatSpec {
  object parsers extends StringLineParsers

  "heading_parser" should "match any number of # and return a HeadingLine" in {

    for (n <- 1 to 6) {
      val input = s"${"#" * n} text that is a heading"
      val heading_line = parsers.parseAll(parsers.headingParser, input).get

      assert(heading_line.text == "text that is a heading")
      assert(heading_line.headerLevel == n, s"$input, $n, $heading_line")
    }
  }

  "code_start" should "match {{{, returning a CodeStartLine" in {
    val input = "{{{"
    val code_start_line = parsers.parseAll(parsers.codeStart, input).get
    assert(code_start_line.text == "")
  }

  it should "match {{{ and directives, returning a CodeStartLine" in {
    val input = "{{{lang:python|this:false|lines:true"
    val expected_directives = Map("lang" -> "python", "this" -> "false", "lines" -> "true")
    val code_start_line = parsers.parseAll(parsers.codeStart, input).get

    assert(code_start_line.directives == expected_directives)
  }

  "code_end" should "match }}}" in {
    val input = "}}}"
    parsers.parseAll(parsers.codeEnd, input).get
  }

  "quote_line" should "match >>>" in {
    val input = "> a quote goes here"
    val quote_line = parsers.parseAll(parsers.quoteParser, input).get

    assert(quote_line.payload == "a quote goes here")
  }

  it should "require a following space" in {
    val input = ">not quote"
    val the_line = parsers.parseAll(parsers.quoteParser, input)

    assert(the_line.isEmpty)
  }

  "unordered_line" should "match *" in {
    val input = "* unordered line!"
    val unordered_line = parsers.parseAll(parsers.unorderedListParser, input).get

    assert(unordered_line.payload == "unordered line!")
  }

  it should "require a following space" in {
    val input = "*not unordered*"
    val the_line = parsers.parseAll(parsers.unorderedListParser, input)

    assert(the_line.isEmpty)
  }

  "ordered_line" should "match any number followed by a period" in {
    for (n <- 0 to 100) {
      val input = s"$n. derpy derpy"
      val ordered_line = parsers.parseAll(parsers.orderedListParser, input).get

      assert(ordered_line.payload == "derpy derpy")
    }
  }

  it should "match a '-' followed by a space" in {
    val input = "- derptastic"
    val ordered_line = parsers.parseAll(parsers.orderedListParser, input).get

    assert(ordered_line.payload == "derptastic")
    assert(ordered_line.glyph == "-")
  }

  it should "require a following space" in {
    val input = "1.not ordered*"
    val the_line = parsers.parseAll(parsers.orderedListParser, input)

    assert(the_line.isEmpty)
  }

  "embed_line" should "match a line that starts with :" in {
    val input = "::image http://haaksmash.com/myhumbs.jpg \"nobody move\"::"
    val embed_line = parsers.parseAll(parsers.embedParser, input).get

    assert(embed_line.text == "image http://haaksmash.com/myhumbs.jpg \"nobody move\"")
    assert(embed_line.arguments == Seq("image", "http://haaksmash.com/myhumbs.jpg", "\"nobody", "move\""))
  }

  it should "support meta" in {
    val input = "::image myhumbs.jpg \"nobody move\"::[class:red]"
    val embed_line = parsers.parseAll(parsers.embedParser, input).get

    assert(embed_line.text == "image myhumbs.jpg \"nobody move\"")
    assert(embed_line.arguments == Seq("image", "myhumbs.jpg", "\"nobody", "move\""))
    assert(embed_line.meta == Map("class" -> "red"))
  }
}
