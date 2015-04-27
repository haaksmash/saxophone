package com.haaksmash.saxophone.parsers

import org.scalatest._

class UtilParserSpec extends FlatSpec {

  object parsers extends UtilParsers

  "rest" should "match everything" in {
    val result = parsers.parseAll(parsers.rest, "eidfccegigculufeefhejggebukccficvegjhelkiegv").get

    assert(result == "eidfccegigculufeefhejggebukccficvegjhelkiegv")
  }

  it should "match the empty string" in {
    val result = parsers.parseAll(parsers.rest, "").get

    assert(result == "")

  }

}
