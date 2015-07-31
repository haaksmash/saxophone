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

  "metadata" should "match things between '[]'" in {
    val input = "[some:stuff|that:makes|meta:data]"
    val result = parsers.parseAll(parsers.metadata, input).get

    assert(result == Map("some" -> "stuff", "that" -> "makes", "meta" -> "data"))
  }

  it should "only take the first of a : pair" in {
    val input = "[style:color:red;font-weight:bold;]"
    val result = parsers.parseAll(parsers.metadata, input).get

    assert(result == Map("style" -> "color:red;font-weight:bold;"))
  }


}
