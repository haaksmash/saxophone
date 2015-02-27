package com.haaksmash.hst

import scala.util.parsing.combinator._

trait UtilParsers extends RegexParsers {
  /*
   * Matches everything in the input string up to the end, including the empty string.
   * Returns the matched string.
   */
  val rest:Parser[String] = Parser {in =>
    if (in.atEnd)
      Success("", in)
    else {
      Success(
        in.source.subSequence(in.offset, in.source.length).toString,
        in.drop(in.source.length - in.offset)
      )
    }
  }
}