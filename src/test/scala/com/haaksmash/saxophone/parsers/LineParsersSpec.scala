package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.primitives.TextLine
import com.haaksmash.saxophone.readers.StringLineReader
import org.scalatest.FlatSpec

class LineParsersSpec extends FlatSpec {
  object parsers extends LineParsers

  "line_token" should "make a textline out of '{' and '}'" in {

    val input = "{\nredleather\n}"

    val result = parsers.lines(new StringLineReader(input)).get

    assert(result == Seq(TextLine("{"), TextLine("redleather"), TextLine("}")))
  }
}
