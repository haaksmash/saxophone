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

import com.haaksmash.saxophone.primitives._


/**
 * Translates [[String]] to specific instances of [[com.haaksmash.saxophone.Line]]
 */
trait StringLineParsers extends UtilParsers {

  val HEADING_GLYPH = "#"
  val CODE_START = "{{{"
  val CODE_END = "}}}"
  val QUOTE_LINE = ">>>"

  val headingParser: Parser[HeadingLine] = s"${HEADING_GLYPH}+ ".r ~ rest ^^ {
    case glyphs ~ text =>
      // glyphs will end with a space that we want to throw away
      HeadingLine(glyphs.trim, text)
  }

  val codeStart: Parser[CodeStartLine] = CODE_START ~> rest ^^ {
    case directives =>
      val directive_map = if (!directives.isEmpty) {
        directives.split('|') map {pair =>
          val keyval = pair.split(':')
          keyval(0) -> keyval(1)
        } toMap
      } else {
        Map[String,String]()
      }

      CodeStartLine(
        directives = directive_map
      )
  }

  val codeEnd: Parser[CodeEndLine] = CODE_END ^^^ {CodeEndLine()}

  val quoteParser: Parser[QuoteLine] = s"$QUOTE_LINE\\s?".r ~ rest ^^ {case leader ~ text => QuoteLine(leader + text)}

  val unorderedListParser: Parser[UnorderedLine] = "\\*\\s?".r ~ rest ^^ {case leader ~ text => UnorderedLine(leader, text)}

  val orderedListParser: Parser[OrderedLine] = ("\\d+\\.\\s?".r | "- ") ~ rest ^^ {case leader ~ text => OrderedLine(leader.trim, text)}

}

