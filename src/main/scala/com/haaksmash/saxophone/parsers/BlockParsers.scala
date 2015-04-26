package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.primitives._

import scala.util.parsing.combinator.Parsers

/**
  * The block parsers translate com.haaksmash.saxophone.Line -> com.haaksmash.saxophone.Node
  */
class BlockParsers extends Parsers {
  type Elem = Line

  /**
   * Matches & consumes a line of type T. Notably, it will __not__ match lines that
   * are subtypes of T.
   *
   * @param c the specific class of Line @tparam T
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
      println(text_lines)
      val text = text_lines.foldLeft("")((s, l) => s + " " + l.payload).trim
      val parsed_text = InlineParsers.parseAll(InlineParsers.elements(Set.empty), text).get
      Paragraph(parsed_text)
  }

  private def flatten_list_lines[T <: ListLine](line_object_apply: ((String, String) => T))(in: List[Line], accumulator: List[Line]): List[Line] = in match {
    case (x: T @unchecked) :: (y: TextLine) :: ys =>
      val new_line = line_object_apply(x.glyph, x.payload + " " + y.payload)
      flatten_list_lines(line_object_apply)(new_line :: ys, accumulator)
    case x :: xs =>
      flatten_list_lines(line_object_apply)(xs, x :: accumulator)
    case Nil => accumulator
  }

  private def fold_text_lines_into_ordered_lines(in:List[Line]) = {
    flatten_list_lines[OrderedLine](OrderedLine.apply)(in, Nil:List[Line]).reverse
  }

  private def fold_text_lines_into_unordered_lines(in:List[Line]) = {
    flatten_list_lines[UnorderedLine](UnorderedLine.apply)(in, Nil:List[Line]).reverse
  }

  val ordered_list_node: Parser[OrderedList] = (line(classOf[OrderedLine]) | line(classOf[TextLine]) ).+ <~ line(classOf[EmptyLine]).? ^^ {
    case line_items =>
      OrderedList(
        fold_text_lines_into_ordered_lines(line_items) map {li => StandardText(li.payload)}
      )
  }

  val unordered_list_node: Parser[UnorderedList] = (line(classOf[UnorderedLine]) | line(classOf[TextLine])).+ <~ line(classOf[EmptyLine]).? ^^ {
    case line_items =>
      UnorderedList(
        (fold_text_lines_into_unordered_lines(line_items) map {li => StandardText(li.payload)}).toSet
      )
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
      println(first_line)
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