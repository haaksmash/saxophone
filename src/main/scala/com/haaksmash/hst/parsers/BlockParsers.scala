package com.haaksmash.hst

import scala.util.parsing.combinator.Parsers

/**
  * The block parsers translate com.haaksmash.hst.Line -> com.haaksmash.hst.Node
  */
class BlockParser extends Parsers {
  type Elem = Line

  def line[T](c:Class[T]): Parser[T] = Parser { in =>
    if (in.first.getClass == c) Success(in.first.asInstanceOf[T], in.rest)
    else {
      Failure(s"not a fitting line", in)
    }
  }

  /**
   * Parses a line of any type *but* T
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
    case h => new Heading(h.headerLevel, Seq(new StandardText(h.payload)))
  }

  val paragraph: Parser[Paragraph] = line(classOf[TextLine]).+ ^^ {
    case text_lines => new Paragraph(
      Seq(new StandardText(
        ("" /: text_lines) ((s, l) => s + " " + l.payload).trim
      ))
    )
  }

  def fold_text_lines_into_ordered_lines(in:List[Line]) = {
    def flatten_list_lines_helper(in: List[Line], accum: List[Line]): List[Line] = in match {
      case (x: OrderedLine) :: (y: TextLine) :: ys => {
        val new_ordered_line = OrderedLine(x.text + " " + y.text)
        flatten_list_lines_helper(new_ordered_line :: ys, accum)
      }
      case x :: xs =>
        flatten_list_lines_helper(xs, x :: accum)
      case Nil => accum
    }

    flatten_list_lines_helper(in, Nil:List[Line]).reverse
  }

  def fold_text_lines_into_unordered_lines(in:List[Line]) = {
    def flatten_list_lines_helper(in: List[Line], accum: List[Line]): List[Line] = in match {
      case (x: UnorderedLine) :: (y: TextLine) :: ys => {
        val new_ordered_line = UnorderedLine(x.text + " " + y.text)
        flatten_list_lines_helper(new_ordered_line :: ys, accum)
      }
      case x :: xs => flatten_list_lines_helper(xs, x :: accum)
      case Nil => accum
    }

    flatten_list_lines_helper(in, Nil:List[Line]) reverse
  }

  val ordered_list_node: Parser[OrderedList] = (line(classOf[OrderedLine]) | line(classOf[TextLine]) ).+ <~ line(classOf[EmptyLine]) ^^ {
    case line_items =>
      new OrderedList(
        fold_text_lines_into_ordered_lines(line_items) map {li => new StandardText(li.payload)}
      )
  }

  val unordered_list_node: Parser[UnorderedList] = (line(classOf[UnorderedLine]) | line(classOf[TextLine])).+ <~ line(classOf[EmptyLine]) ^^ {
    case line_items =>
      new UnorderedList(
        fold_text_lines_into_unordered_lines(line_items) map {li => new StandardText(li.payload)} toSet
      )
  }

  val code_node: Parser[Code] = (line(classOf[CodeStartLine]) ~ notLine(classOf[CodeEndLine]).+ ~ line(classOf[CodeEndLine])) ^^ {
    case start ~ code ~ end =>
      val code_strings = code map {
        case l: EmptyLine => "\n"
        case l:Line => l.text
      }
      new Code(
        start.directives,
        code_strings.foldLeft("")((acc, newline) => acc ++ newline)
      )
  }

  val quote_source: Parser[StandardText] = Parser { in =>
    if (!in.first.isInstanceOf[TextLine])
      Failure("not a standard text line", in)
    else if (!in.first.text.startsWith("["))
      Failure("not a source line (wrong start char)", in)
    else if (!in.first.text.endsWith("]"))
      Failure("not a source line (wrong end char)", in)
    else
      Success(
        // Strip out the leading "[" and trailing "]"; they're purely syntactic
        StandardText(in.first.text.substring(1, in.first.text.length - 1)),
        in.rest
      )
  }

  val quote_node: Parser[Quote] = (line(classOf[QuoteLine])).+ ~ quote_source.? ^^ {
    case quotes ~ source =>
      Quote(quotes map { case quote_line => new StandardText(quote_line.payload)}, source)
  }

  val nodes: Parser[Node] = Parser { in =>
    if (in.atEnd)
      Failure("end of input", in)
    else {
      val first_line = in.first
      val results = first_line match {
        case l:HeadingLine =>
          println("headernode")
          header_node(in)
        case l:OrderedLine =>
          println("ordered lines")
          ordered_list_node(in)
        case l:UnorderedLine =>
          println("unordered lines")
          unordered_list_node(in)
        case l:CodeStartLine =>
          println("codes")
          code_node(in)
        case l:QuoteLine =>
          println("quote")
          quote_node(in)
        case _ =>
          println("paragraph")
          paragraph(in)
      }
      results
    }
  }

  /*
  the blocks parser simply strips out all EmptyLines after a matching nodes group of lines
   */
  val blocks: Parser[Document] = (nodes <~ (line(classOf[EmptyLine])).*).+ ^^ { Document(_) }

  val stringTokenizer = new StringLineTokenizer

  def eval(input: String)  = {
    val lines = stringTokenizer.eval(input)
    println("parsing Lines into Nodes")
    val document = lines map {
      l => blocks(new LineReader(l))
    }

    document
  }
}