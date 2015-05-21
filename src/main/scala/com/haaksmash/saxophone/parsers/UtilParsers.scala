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