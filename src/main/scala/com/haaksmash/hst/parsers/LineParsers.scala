package com.haaksmash.hst

import scala.util.parsing.combinator._

/**
 * StringLineTokenizer translates String -> Line
 */
class StringLineTokenizer extends Parsers {
  type Elem = String
  object lineParsers extends StringLineParsers

  /*
   * Stupid hack so this tokenizer can use StringLineParsers parsers as if they were its own.
   */
  def p[T](parser:lineParsers.Parser[T]):Parser[T] = Parser {in =>
    if (in.atEnd)
      Failure("End of input in "+ parser, in)
    else {
      lineParsers.parseAll(parser, in.first) match {
        case lineParsers.Success(t, _) => Success(t.asInstanceOf[T], in.rest)
        case n:lineParsers.NoSuccess => Failure(n.msg, in)
      }
    }
  }

  val lineToken:Parser[Line] = Parser {in =>
    if (in.atEnd)
      Failure("End of input in lineToken", in)
    else {
      val line = in.first
      val char = if (line.isEmpty) '\n' else line.charAt(0)

      char match {
        case '#' => p(lineParsers.headingParser)(in)
        case '*' => p(lineParsers.unorderedListParser)(in)
        case '\n' => Success(EmptyLine(), in.rest)
        case n if ('0' <= n && n <= '9') => p(lineParsers.orderedListParser)(in)
        case '>' => p(lineParsers.quoteParser)(in)
        case '{' => p(lineParsers.codeStart)(in)
        case '}' => p(lineParsers.codeEnd)(in)
        case _ => Success(TextLine(line), in.rest)
      }
    }
  }

  val lines: Parser[Seq[Line]] = lineToken.*

  def eval(input:String) = {
    lines(new StringLineReader(input)) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) => {
        println("Failure: " + msg)
        None
      }
      case Error(msg, _) => {
        println("Error: " + msg)
        None
      }
    }
  }
}