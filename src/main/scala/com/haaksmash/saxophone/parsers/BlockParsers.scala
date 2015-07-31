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
import com.haaksmash.saxophone.readers.LineReader

import scala.util.parsing.combinator.Parsers

/**
 * The block parsers translate [[com.haaksmash.saxophone.primitives.Line]] to [[com.haaksmash.saxophone.primitives.Node]]
 */
class BlockParsers extends Parsers {
  type Elem = Line

  val ORDERED_AS_UNORDERED_GLYPH = "-"

  /**
   * Matches & consumes a line of type T. Notably, it will __not__ match lines that
   * are subtypes of T.
   *
   * @param c the specific class of Line to match
   * @tparam T the type of the Line to match (usually inferred)
   */
  def line[T](c: Class[T]): Parser[T] = Parser { in =>
    if (in.first.getClass == c) Success(in.first.asInstanceOf[T], in.rest)
    else {
      Failure(s"not a fitting line", in)
    }
  }

  /**
   * Matches & consumes a line of any type __except for__ T. the in-library `not` combinator
   * isn't sufficient because it interacts incorrectly with the `line` combinator
   * defined here.
   *
   * @param c the specific class of Line to *not* match.
   * @tparam T the type of the Line not to match (usually inferred)
   */
  def notLine[T](c: Class[T]): Parser[Line] = Parser { in =>
    if (in.atEnd)
      Failure("At end of input.", in)
    else if (in.first.getClass == c)
      Failure("Not a fitting line.", in)
    else
      Success(in.first, in.rest)
  }

  lazy val header_node: Parser[Heading] = line(classOf[HeadingLine]) ^^ {
    case h => Heading(h.headerLevel, Seq(StandardText(h.payload)))
  }

  lazy val paragraph: Parser[Paragraph] = line(classOf[TextLine]).+ ^^ {
    case text_lines =>
      val text = text_lines.map(_.payload).mkString(" ").trim
      val parsed_text = InlineParsers.parseAll(InlineParsers.elements(Set.empty), text).get
      Paragraph(parsed_text)
  }

  lazy val ordered_list_node: Parser[OrderedList] = Parser { in =>
    if (in.atEnd)
      Failure("End of input", in)
    else if (!in.first.isInstanceOf[OrderedLine])
      Failure("not an ordered line", in)
    else {
      var input = in
      var items = Seq[Seq[Node]]()
      val present_unordered = input.first match {
        case line: OrderedLine =>
          line.glyph == ORDERED_AS_UNORDERED_GLYPH
        case _ => false
      }
      while (input.first.isInstanceOf[OrderedLine]) {
        val leading_line = input.first.asInstanceOf[OrderedLine]
        // This is unfortunate; I'd like to use the `source` method on Reader[T], but that
        // has to return a CharSequence, which this Reader[Line] obviously cannot.
        val line_in = input.rest.asInstanceOf[LineReader].lines

        val sublines = line_in.takeWhile {
          // never scoop another OrderedLine into this item; it denotes the start of the NEXT item
          case next: OrderedLine => false
          // scoop TextLines into this item no matter what
          case next: TextLine => true
          // don't scoop anything else
          case next => false
        }

        items = items ++ Seq(
        {
          if (sublines.nonEmpty)
            Seq(
              nodes(new LineReader(TextLine(leading_line.payload) +: sublines)).get
            )
          else
            Seq(nodes(new LineReader(Seq(TextLine(leading_line.payload)))).get)
        }
        )

        input = input.rest.drop(sublines.length)
      }

      val o = OrderedList(
        items,
        present_unordered
      )
      Success(o, input.rest)
    }
  }

  lazy val unordered_list_node: Parser[UnorderedList] = Parser { in =>
    if (in.atEnd)
      Failure("End of input", in)
    else if (!in.first.isInstanceOf[UnorderedLine])
      Failure("not an ordered line", in)
    else {
      var input = in
      var items = Set[Seq[Node]]()
      while (input.first.isInstanceOf[UnorderedLine]) {
        val leading_line = input.first.asInstanceOf[UnorderedLine]
        // This is unfortunate; I'd like to use the `source` method on Reader[T], but that
        // has to return a CharSequence, which this Reader[Line] obviously cannot.
        val line_in = input.rest.asInstanceOf[LineReader].lines

        val sublines = line_in.takeWhile {
          // never scoop another OrderedLine into this item; it denotes the start of the NEXT item
          case next: OrderedLine => false
          // scoop TextLines into this item no matter what
          case next: TextLine => true
          // don't scoop anything else
          case next => false
        }

        items = items ++ Set(
        {
          if (sublines.nonEmpty)
            Seq(
              nodes(new LineReader(TextLine(leading_line.payload) +: sublines)).get
            )
          else
            Seq(nodes(new LineReader(Seq(TextLine(leading_line.payload)))).get)
        }
        )

        input = input.rest.drop(sublines.length)
      }

      val o = UnorderedList(items)
      Success(o, input.rest)
    }
  }

  lazy val code_node: Parser[Code] = (line(classOf[CodeStartLine]) ~ notLine(classOf[CodeEndLine])
    .+ <~ line(classOf[CodeEndLine])) ^^ {
    case start ~ code =>
      val code_strings = code map {
        case l: EmptyLine => "\n"
        case HeadingLine(prefix, text) => s"$prefix $text"
        case l: Line => l.text
      }
      Code(
        start.directives,
        code_strings.mkString("\n")
      )
  }

  lazy val quote_source: Parser[Seq[InlineNode]] = Parser { in =>
    if (!in.first.isInstanceOf[TextLine])
      Failure("not a standard text line", in)
    else if (!in.first.text.startsWith("["))
      Failure("not a source line (wrong start char)", in)
    else if (!in.first.text.endsWith("]"))
      Failure("not a source line (wrong end char)", in)
    else
      Success(
        // Strip out the leading "[" and trailing "]"; they're purely syntactic
        InlineParsers
          .parseAll(InlineParsers.elements(Set.empty), in.first.text.substring(1, in.first.text.length - 1))
          .get,
        in.rest
      )
  }

  lazy val quote_node: Parser[Quote] = line(classOf[QuoteLine]).+ ~ quote_source.? ^^ {
    case quotes ~ source =>
      Quote(
        InlineParsers.parseAll(
          InlineParsers.elements(Set.empty),
          quotes.map(_.payload).mkString(" ")
        ).get,
        source
      )
  }

  lazy val embed_node: Parser[EmbedNode] = line(classOf[EmbedLine]) ^? ({
    case embed_line if EmbedNode.VALID_EMBED_TYPES.contains(embed_line.arguments.head) =>
      EmbedNode.VALID_EMBED_TYPES.get(embed_line.arguments.head).map(
        f => f(embed_line.arguments.tail, embed_line.meta)
      ).get
  }, line => {
    s"unrecognized embed type: ${line.arguments.head}; known types: [${EmbedNode.VALID_EMBED_TYPES.keys.mkString(", ")}]"
  })

  lazy val nodes: Parser[Node] = Parser { in =>
    if (in.atEnd)
      Failure("end of input", in)
    else {
      val first_line = in.first
      val results = first_line match {
        case l: HeadingLine =>
          header_node(in)
        case l: OrderedLine =>
          ordered_list_node(in)
        case l: UnorderedLine =>
          unordered_list_node(in)
        case l: CodeStartLine =>
          code_node(in)
        case l: QuoteLine =>
          quote_node(in)
        case l: EmbedLine =>
          embed_node(in)
        case _ =>
          paragraph(in)
      }
      results
    }
  }

  /**
   * strips out all EmptyLines after a matching nodes group of lines
   */
  val blocks: Parser[Document] = (nodes <~ line(classOf[EmptyLine]).*).+ ^^ {
    Document(_)
  }
}
