package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.{CodeStartLine, HeadingLine, StringLineParsers}
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
    val code_end_line = parsers.parseAll(parsers.codeEnd, input).get
  }

  "quote_line" should "match >>>" in {
    val input = ">>> a quote goes here"
    val quote_line = parsers.parseAll(parsers.quoteParser, input).get

    assert(quote_line.text == "a quote goes here")
  }

  "unordered_line" should "match *" in {
    val input = "* unorderd line!"
    val unordered_line = parsers.parseAll(parsers.unorderedListParser, input).get

    assert(unordered_line.text == "unorderd line!")
  }

  "ordered_line" should "match any number followed by a period" in {
    for (n <- 0 to 100) {
      val input = s"$n. derpy derpy"
      val ordered_line = parsers.parseAll(parsers.orderedListParser, input).get

      assert(ordered_line.text == "derpy derpy")
    }
  }

}
