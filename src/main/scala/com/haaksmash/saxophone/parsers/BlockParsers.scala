package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.primitives._
import com.haaksmash.saxophone.readers.LineReader

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

/**
  * The block parsers translate com.haaksmash.saxophone.Line -> com.haaksmash.saxophone.Node
  */
class BlockParsers extends Parsers {
  type Elem = Line

  /**
   * Matches & consumes a line of type T. Notably, it will __not__ match lines that
   * are subtypes of T.
   *
   * @param c the specific class of Line to match
   * @tparam T the type of the Line to match (usually inferred)
   */
  def line[T](c:Class[T]): Parser[T] = Parser { in =>
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
  def notLine[T](c:Class[T]):Parser[Line] = Parser {in =>
    if (in.atEnd)
      Failure("At end of input.", in)
    else if (in.first.getClass == c)
      Failure("Not a fitting line.", in)
    else
      Success(in.first, in.rest)
  }

  val header_node: Parser[Heading] = line(classOf[HeadingLine]) ^^ {
    case h => Heading(h.headerLevel, Seq(StandardText(h.payload)))
  }

  val paragraph: Parser[Paragraph] = line(classOf[TextLine]).+ ^^ {
    case text_lines =>
      val text = text_lines.map(_.payload).mkString(" ").trim
      val parsed_text = InlineParsers.parseAll(InlineParsers.elements(Set.empty), text).get
      Paragraph(parsed_text)
  }

  val ordered_list_node: Parser[OrderedList] = Parser { in =>
    if (in.atEnd)
      Failure("End of input", in)
    else if (!in.first.isInstanceOf[OrderedLine])
      Failure("not an ordered line", in)
    else {
        var input = in
        var items = Seq[Seq[Node]]()
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

          items = items ++ Seq({
            if (sublines.length > 0)
              Seq(
                nodes(new LineReader(TextLine(leading_line.payload) +: sublines)).get
              )
            else
              Seq(nodes(new LineReader(Seq(TextLine(leading_line.payload)))).get)
          })

          input = input.rest.drop(sublines.length)
        }

        val o = OrderedList(items)
        Success(o, input.rest)
    }
  }

  val unordered_list_node: Parser[UnorderedList] = Parser { in =>
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

          items = items ++ Set({
            if (sublines.length > 0)
              Seq(
                nodes(new LineReader(TextLine(leading_line.payload) +: sublines)).get
              )
            else
              Seq(nodes(new LineReader(Seq(TextLine(leading_line.payload)))).get)
          })

          input = input.rest.drop(sublines.length)
        }

        val o = UnorderedList(items)
        Success(o, input.rest)
    }
  }

  val code_node: Parser[Code] = (line(classOf[CodeStartLine]) ~ notLine(classOf[CodeEndLine]).+ <~ line(classOf[CodeEndLine])) ^^ {
    case start ~ code =>
      val code_strings = code map {
        case l: EmptyLine => "\n"
        case l:Line => l.text
      }
      Code(
        start.directives,
        code_strings.mkString("\n")
      )
  }

  val quote_source: Parser[Seq[InlineNode]] = Parser { in =>
    if (!in.first.isInstanceOf[TextLine])
      Failure("not a standard text line", in)
    else if (!in.first.text.startsWith("["))
      Failure("not a source line (wrong start char)", in)
    else if (!in.first.text.endsWith("]"))
      Failure("not a source line (wrong end char)", in)
    else
      Success(
        // Strip out the leading "[" and trailing "]"; they're purely syntactic
        InlineParsers.parseAll(InlineParsers.elements(Set.empty), in.first.text.substring(1, in.first.text.length - 1)).get,
        in.rest
      )
  }

  val quote_node: Parser[Quote] = line(classOf[QuoteLine]).+ ~ quote_source.? ^^ {
    case quotes ~ source =>
      Quote(
        InlineParsers.parseAll(
          InlineParsers.elements(Set.empty),
          quotes.map(_.payload).mkString(" ")
        ).get,
        source
      )
  }

  val nodes: Parser[Node] = Parser { in =>
    if (in.atEnd)
      Failure("end of input", in)
    else {
      val first_line = in.first
      val results = first_line match {
        case l:HeadingLine =>
          header_node(in)
        case l:OrderedLine =>
          ordered_list_node(in)
        case l:UnorderedLine =>
          unordered_list_node(in)
        case l:CodeStartLine =>
          code_node(in)
        case l:QuoteLine =>
          quote_node(in)
        case _ =>
          paragraph(in)
      }
      results
    }
  }

  /**
   * strips out all EmptyLines after a matching nodes group of lines
   */
  val blocks: Parser[Document] = (nodes <~ line(classOf[EmptyLine]).*).+ ^^ { Document(_) }
}
