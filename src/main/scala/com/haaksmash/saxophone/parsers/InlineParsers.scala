package com.haaksmash.saxophone

import scala.util.parsing.combinator.RegexParsers

object InlineParsers extends RegexParsers {

  val special_chars = Set(
    '{',
    '[',
    '*',
    '/',
    '~',
    '_',
    '`'
  )

  def aChar = Parser{ in =>
    if (in.atEnd) {
      Failure("End of input reached.", in)
    } else {
      Success(in.first, in.rest)
    }
  }

  val standard_text: Parser[StandardText] = Parser{ in =>
    if (in.atEnd)
      Failure("End of input reached.", in)
    else if (special_chars.contains(in.first))
      Failure("not standard text", in)
    else {
      val subsource = in.source.subSequence(in.offset, in.source.length)
      val regular_text = subsource.toString.toList.takeWhile(!special_chars.contains(_)).mkString
      Success(StandardText(regular_text), in.drop(regular_text.length))
    }
  }

  val emphasized_text: Parser[EmphasizedText] = "/" ~> (not("/") ~> aChar+) <~ "/" ^^ {
    case chars => EmphasizedText(chars.mkString)
  }

  val weighted_text: Parser[WeightedText] = "*" ~> (not("*") ~> aChar+) <~ "*" ^^ {
    // For now, only support a single level of added-weight
    case text => WeightedText(1, text.mkString)
  }

  val underlined_text: Parser[UnderlinedText] = "_" ~> (not("_") ~> aChar+) <~ "_" ^^ {
    case chars => UnderlinedText(chars.mkString)
  }

  val struckthrough_text: Parser[StruckthroughText] = "~" ~> (not("~") ~> aChar+) <~ "~" ^^ {
    case chars => StruckthroughText(chars.mkString)
  }

  val monospaced_text: Parser[MonospaceText] = "`" ~> (not("`") ~> aChar+) <~ "`" ^^ {
    case chars => MonospaceText(chars.mkString)
  }

  val link_text: Parser[Link] = ("[" ~> (not("]")~> aChar+) <~ "]") ~ ("(" ~> (not(")") ~> aChar+) <~ ")").? ^^ {
    case text ~ maybe_target => Link(
      parse(elements(Set("a")), text.mkString).get,
      maybe_target match {
        case Some(chars) => LinkTarget(chars.mkString)
        case None => LinkTarget(text.mkString)
      }
    )
  }

  val footnote_text: Parser[Footnote] = "{" ~> (not("}") ~> aChar+) <~ "}" ^^ {
    case text => Footnote(parse(elements(Set("f")), text.mkString).get)
  }

  def element(visited: Set[String]): Parser[InlineNode] = Parser { in =>
    if (in.atEnd)
      Failure("End of input.", in)
    else {
      in.first match {
        case '{' =>
          if (visited.contains("f"))
            Failure("can't nest footnotes", in)
          else
            footnote_text(in)
        case '[' =>
          if (visited.contains("a"))
            Failure("can't nest links", in)
          else
            link_text(in)
        case '*' =>
          if (visited.contains("b"))
            Failure("can't nest weights", in)
          else
            weighted_text(in)
        case '/' =>
          if (visited.contains("i"))
            Failure("can't nest italics", in)
          else
            emphasized_text(in)
        case '~' => struckthrough_text(in)
        case '_' => underlined_text(in)
        case '`' => monospaced_text(in)
        case _ => Failure(s"didn't recognize symbol ${in.first}", in)
      }
    }
  }

  def elements(visited: Set[String] = Set.empty[String]): Parser[Seq[InlineNode]] = (element(visited) | standard_text).*
}
