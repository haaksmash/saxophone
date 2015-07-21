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

import com.haaksmash.saxophone.primitives.{TextLine, EmptyLine, Line}
import com.haaksmash.saxophone.readers.StringLineReader

import scala.util.parsing.combinator._

/**
 * Provides parsers that translate [[String]] -> [[com.haaksmash.saxophone.primitives.Line]]
 */
class LineParsers extends Parsers {
  type Elem = String
  object line_parsers extends StringLineParsers

  /**
   * Stupid hack so this tokenizer can use [[com.haaksmash.saxophone.parsers.StringLineParsers]]
   * parsers as if they were its own.
   */
  private def delegateParsing[T](parser:line_parsers.Parser[T]):Parser[T] = Parser {in =>
    if (in.atEnd)
      Failure("End of input in "+ parser, in)
    else {
      line_parsers.parseAll(parser, in.first) match {
        case line_parsers.Success(t, _) => Success(t.asInstanceOf[T], in.rest)
        case n:line_parsers.NoSuccess => Failure(n.msg, in)
      }
    }
  }

  val line_token:Parser[Line] = Parser {in =>
    if (in.atEnd)
      Failure("End of input in line_token", in)
    else {
      val line = in.first
      val char = if (line.isEmpty) '\n' else line.charAt(0)

      val maybe_result = char match {
        case '#' => delegateParsing(line_parsers.headingParser)(in)
        case '*' => delegateParsing(line_parsers.unorderedListParser)(in)
        case '\n' => Success(EmptyLine(), in.rest)
        case n if '0' <= n && n <= '9' => delegateParsing(line_parsers.orderedListParser)(in)
        case '-' => delegateParsing(line_parsers.orderedListParser)(in)
        case '>' => delegateParsing(line_parsers.quoteParser)(in)
        case '{' => delegateParsing(line_parsers.codeStart)(in)
        case '}' => delegateParsing(line_parsers.codeEnd)(in)
        case _ => Failure("no matching start char", in)
      }

      // A line of text is *always* going to be a TextLine, if nothing else.
      // Even if it starts with a suspcicious character and that doesn't pan
      // out.
      maybe_result match {
        case Failure(_, _) => Success(TextLine(line), in.rest)
        case x => x
      }
    }
  }

  val lines: Parser[Seq[Line]] = line_token.*

  def eval(input:String) = {
    lines(new StringLineReader(input)) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) =>
        println("Failure: " + msg)
        None
      case Error(msg, _) =>
        println("Error: " + msg)
        None
    }
  }
}
