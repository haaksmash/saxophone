package com.haaksmash.saxophone

import scala.util.parsing.combinator.RegexParsers

object InlineParsers extends RegexParsers {

  val FOOTNOTE_START = '{'
  val FOOTNOTE_END = '}'
  val LINK_START = '['
  val LINK_END = ']'
  val LINK_TARGET_START = '('
  val LINK_TARGET_END = ')'
  val WEIGHTED_START = '*'
  val EMPHASIZED_START = '/'
  val STRUCKTHROUGH_START = '~'
  val UNDERLINE_START = '_'
  val MONOSPACE_START = '`'

  val special_chars = Set(
    FOOTNOTE_START,
    LINK_START,
    WEIGHTED_START,
    EMPHASIZED_START,
    STRUCKTHROUGH_START,
    UNDERLINE_START,
    MONOSPACE_START
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

  val emphasized_text: Parser[EmphasizedText] = EMPHASIZED_START ~> (not(EMPHASIZED_START) ~> aChar+) <~ EMPHASIZED_START ^^ {
    case chars => EmphasizedText(chars.mkString)
  }

  val weighted_text: Parser[WeightedText] = WEIGHTED_START ~> (not(WEIGHTED_START) ~> aChar+) <~ WEIGHTED_START ^^ {
    // For now, only support a single level of added-weight
    case text => WeightedText(1, text.mkString)
  }

  val underlined_text: Parser[UnderlinedText] = UNDERLINE_START ~> (not(UNDERLINE_START) ~> aChar+) <~ UNDERLINE_START ^^ {
    case chars => UnderlinedText(chars.mkString)
  }

  val struckthrough_text: Parser[StruckthroughText] = STRUCKTHROUGH_START ~> (not(STRUCKTHROUGH_START) ~> aChar+) <~ STRUCKTHROUGH_START ^^ {
    case chars => StruckthroughText(chars.mkString)
  }

  val monospaced_text: Parser[MonospaceText] = MONOSPACE_START ~> (not(MONOSPACE_START) ~> aChar+) <~ MONOSPACE_START ^^ {
    case chars => MonospaceText(chars.mkString)
  }

  val link_text: Parser[Link] = (LINK_START ~> (not(LINK_END)~> aChar+) <~ LINK_END) ~
    (LINK_TARGET_START ~> (not(LINK_TARGET_END) ~> aChar+) <~ LINK_TARGET_END).? ^^ {
    case text ~ maybe_target => Link(
      parse(elements(Set("a")), text.mkString).get,
      maybe_target match {
        case Some(chars) => LinkTarget(chars.mkString)
        case None => LinkTarget(text.mkString)
      }
    )
  }

  val footnote_text: Parser[Footnote] = FOOTNOTE_START ~> (not(FOOTNOTE_END) ~> aChar+) <~ FOOTNOTE_END ^^ {
    case text => Footnote(parse(elements(Set("f")), text.mkString).get)
  }

  def element(visited: Set[String]): Parser[InlineNode] = Parser { in =>
    if (in.atEnd)
      Failure("End of input.", in)
    else {
      in.first match {
        case FOOTNOTE_START =>
          if (visited.contains("f"))
            Failure("can't nest footnotes", in)
          else
            footnote_text(in)
        case LINK_START =>
          if (visited.contains("a"))
            Failure("can't nest links", in)
          else
            link_text(in)
        case WEIGHTED_START =>
          if (visited.contains("b"))
            Failure("can't nest weights", in)
          else
            weighted_text(in)
        case EMPHASIZED_START =>
          if (visited.contains("i"))
            Failure("can't nest italics", in)
          else
            emphasized_text(in)
        case STRUCKTHROUGH_START => struckthrough_text(in)
        case UNDERLINE_START => underlined_text(in)
        case MONOSPACE_START => monospaced_text(in)
        case _ => Failure(s"didn't recognize symbol ${in.first}", in)
      }
    }
  }

  def elements(visited: Set[String] = Set.empty[String]): Parser[Seq[InlineNode]] = (element(visited) | standard_text).*
}
