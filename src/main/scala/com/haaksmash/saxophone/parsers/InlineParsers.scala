package com.haaksmash.saxophone

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

  val special_char_to_tracking_and_ending_char = Map(
    FOOTNOTE_START -> ("f", FOOTNOTE_END),
    LINK_START -> ("a", LINK_END),
    WEIGHTED_START -> ("b", WEIGHTED_END),
    EMPHASIZED_START -> ("i", EMPHASIZED_END),
    STRUCKTHROUGH_START -> ("s", STRUCKTHROUGH_END),
    UNDERLINE_START -> ("u", UNDERLINE_END),
    MONOSPACE_START -> ("m", MONOSPACE_END)
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
    else if (special_char_to_tracking_and_ending_char.contains(in.first))
      Failure("not standard text", in)
    else {
      val subsource = in.source.subSequence(in.offset, in.source.length)
      val regular_text = subsource.toString.toList.takeWhile(!special_char_to_tracking_and_ending_char.contains(_)).mkString
      Success(StandardText(regular_text), in.drop(regular_text.length))
    }
  }

  val emphasized_text: Parser[EmphasizedText] = EMPHASIZED_START ~> (not(EMPHASIZED_END) ~> aChar+) <~ EMPHASIZED_END ^^ {
    case chars => EmphasizedText(chars.mkString)
  }

  val weighted_text: Parser[WeightedText] = WEIGHTED_START ~> (not(WEIGHTED_END) ~> aChar+) <~ WEIGHTED_END^^ {
    // For now, only support a single level of added-weight
    case text => WeightedText(1, text.mkString)
  }

  val underlined_text: Parser[UnderlinedText] = UNDERLINE_START ~> (not(UNDERLINE_END) ~> aChar+) <~ UNDERLINE_END ^^ {
    case chars => UnderlinedText(chars.mkString)
  }

  val struckthrough_text: Parser[StruckthroughText] = STRUCKTHROUGH_START ~> (not(STRUCKTHROUGH_END) ~> aChar+) <~ STRUCKTHROUGH_END ^^ {
    case chars => StruckthroughText(chars.mkString)
  }

  val monospaced_text: Parser[MonospaceText] = MONOSPACE_START ~> (not(MONOSPACE_END) ~> aChar+) <~ MONOSPACE_END ^^ {
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
            underlined_text(in)
        case MONOSPACE_START =>
          if (visited contains special_char_to_tracking_and_ending_char(MONOSPACE_START)._1)
            Failure("can't nest monospace", in)
          else
            monospaced_text(in)

        case _ => Failure(s"didn't recognize symbol ${in.first}", in)
      }
    }
  }

  def elements(visited: Set[String] = Set.empty[String]): Parser[Seq[InlineNode]] = (element(visited) | standard_text).*
}
