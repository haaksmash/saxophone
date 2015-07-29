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

import scala.util.parsing.combinator.RegexParsers

object InlineParsers extends RegexParsers {

  val FOOTNOTE_START = '{'
  val FOOTNOTE_END = '}'
  val LINK_START = '['
  val LINK_END = ']'
  val LINK_TARGET_START = '('
  val LINK_TARGET_END = ')'
  val WEIGHTED_START, WEIGHTED_END = '*'
  val EMPHASIZED_START, EMPHASIZED_END = '/'
  val STRUCKTHROUGH_START, STRUCKTHROUGH_END = '~'
  val UNDERLINE_START, UNDERLINE_END = '_'
  val MONOSPACE_START, MONOSPACE_END = '`'
  val RAW_START, RAW_END = '|'

  val special_char_to_tracking_and_ending_char = Map(
    FOOTNOTE_START -> ("f", FOOTNOTE_END),
    LINK_START -> ("a", LINK_END),
    WEIGHTED_START -> ("b", WEIGHTED_END),
    EMPHASIZED_START -> ("i", EMPHASIZED_END),
    STRUCKTHROUGH_START -> ("s", STRUCKTHROUGH_END),
    UNDERLINE_START -> ("u", UNDERLINE_END),
    MONOSPACE_START -> ("m", MONOSPACE_END),
    RAW_START -> ("r", RAW_END)
  )

  def aChar = Parser { in =>
    if (in.atEnd) {
      Failure("End of input reached.", in)
    } else {
      Success(in.first, in.rest)
    }
  }

  def standardText(special: Set[Char]): Parser[StandardText] = Parser { in =>
    if (in.atEnd)
      Failure("End of input reached.", in)
    else {
      val source = in.source
      var pos = in.offset
      val end = source.length()
      val result = new StringBuilder()

      while (pos < end && !special.contains(source.charAt(pos))) {
        val c = source.charAt(pos)
        if (
          c == '\\'
          && pos + 1 < end
          && special_char_to_tracking_and_ending_char.contains(source.charAt(pos + 1))
        ) {
          result.append(source.charAt(pos + 1))
          pos += 2
        } else {
          result.append(source.charAt(pos))
          pos += 1
        }
      }

      val text = result.toString()
      if (text.length == 0)
        Failure("no text consumed", in)
      else
        Success(StandardText(text), in.drop(pos - in.offset))
    }
  }

  val metadata: Parser[Map[String, String]] = "[" ~> ((not("]") ~> aChar).+) <~ "]" ^^ {
    case metas =>
      metas.mkString.split('|')
        .map(metapair => metapair.split(':'))
        .map(l => l(0) -> l(1))
        .toMap
  }

  val raw_text: Parser[RawText] = RAW_START ~> ((not(RAW_END) ~> aChar).+) <~ RAW_END ^^ {
    case chars => RawText(chars.mkString)
  }

  val emphasized_text: Parser[EmphasizedText] = (EMPHASIZED_START ~> ((not(EMPHASIZED_END) ~> aChar).+) <~ EMPHASIZED_END) ~ metadata.? ^^ {
    case chars ~ meta => EmphasizedText(chars.mkString, meta.getOrElse(Map()))
  }

  val weighted_text: Parser[WeightedText] = (WEIGHTED_START ~> ((not(WEIGHTED_END) ~> aChar).+) <~ WEIGHTED_END) ~ metadata.? ^^ {
    // For now, only support a single level of added-weight
    case text ~ meta => WeightedText(1, text.mkString, meta.getOrElse(Map()))
  }

  val marked_text: Parser[MarkedText] = (UNDERLINE_START ~> ((not(UNDERLINE_END) ~> aChar).+) <~ UNDERLINE_END) ~ metadata.? ^^ {
    case chars ~ meta => MarkedText(chars.mkString, meta.getOrElse(Map()))
  }

  val struckthrough_text: Parser[StruckthroughText] = (STRUCKTHROUGH_START ~> ((not(STRUCKTHROUGH_END) ~> aChar).+) <~ STRUCKTHROUGH_END) ~ metadata.? ^^ {
    case chars ~ meta => StruckthroughText(chars.mkString, meta.getOrElse(Map()))
  }

  val monospaced_text: Parser[MonospaceText] = (MONOSPACE_START ~> standardText(Set(MONOSPACE_END)) <~ MONOSPACE_END) ~ metadata.? ^^ {
    case text ~ meta => MonospaceText(text.text, meta.getOrElse(Map()))
  }

  val link_text: Parser[Link] = (LINK_START ~> ((not(LINK_END)~> aChar).+) <~ LINK_END) ~
    (LINK_TARGET_START ~> ((not(LINK_TARGET_END) ~> aChar).+) <~ LINK_TARGET_END).? ^^ {
    case text ~ maybe_target => Link(
      parse(elements(Set("a")), text.mkString).get,
      maybe_target match {
        case Some(chars) => LinkTarget(chars.mkString)
        case None => LinkTarget(text.mkString)
      }
    )
  }

  val footnote_text: Parser[Footnote] = FOOTNOTE_START ~> ((not(FOOTNOTE_END) ~> aChar).+) <~ FOOTNOTE_END ^^ {
    case text => Footnote(parse(elements(Set("f")), text.mkString).get)
  }

  def element(visited: Set[String]): Parser[InlineNode] = Parser { in =>
    if (in.atEnd)
      Failure("End of input.", in)
    else {
      in.first match {
        case FOOTNOTE_START =>
          if (visited contains special_char_to_tracking_and_ending_char(FOOTNOTE_START)._1)
            Failure("can't nest footnotes", in)
          else
            footnote_text(in)
        case LINK_START =>
          if (visited contains special_char_to_tracking_and_ending_char(LINK_START)._1)
            Failure("can't nest links", in)
          else
            link_text(in)
        case WEIGHTED_START =>
          if (visited contains special_char_to_tracking_and_ending_char(WEIGHTED_START)._1)
            Failure("can't nest weights", in)
          else
            weighted_text(in)
        case EMPHASIZED_START =>
          if (visited contains special_char_to_tracking_and_ending_char(EMPHASIZED_START)._1)
            Failure("can't nest italics", in)
          else
            emphasized_text(in)
        case STRUCKTHROUGH_START =>
          if (visited contains special_char_to_tracking_and_ending_char(STRUCKTHROUGH_START)._1)
            Failure("can't nest strikethrough", in)
          else
            struckthrough_text(in)
        case UNDERLINE_START =>
          if (visited contains special_char_to_tracking_and_ending_char(UNDERLINE_START)._1)
            Failure("can't nest underline", in)
          else
            marked_text(in)
        case MONOSPACE_START =>
          if (visited contains special_char_to_tracking_and_ending_char(MONOSPACE_START)._1)
            Failure("can't nest monospace", in)
          else
            monospaced_text(in)
        case RAW_START =>
          if (visited contains special_char_to_tracking_and_ending_char(RAW_START)._1)
            Failure("can't nest raw", in)
          else
            raw_text(in)

        case _ => Failure(s"didn't recognize symbol ${in.first}", in)
      }
    }
  }

  def elements(visited: Set[String] = Set.empty[String]): Parser[Seq[InlineNode]] = (
    element(visited) |
    standardText(special_char_to_tracking_and_ending_char.keys.toSet)
  ).*
}
