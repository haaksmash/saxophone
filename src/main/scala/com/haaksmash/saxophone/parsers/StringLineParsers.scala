package com.haaksmash.saxophone


/**
 * Translates [[String]] to specific instances of [[com.haaksmash.saxophone.Line]]
 */
trait StringLineParsers extends UtilParsers {
  val headingParser: Parser[HeadingLine] = "#+ ".r ~ rest ^^ {
    case glyphs ~ text =>
      // glyphs will end with a space that we want to throw away
      HeadingLine(glyphs.slice(0, glyphs.length - 1), text)
  }

  val codeStart: Parser[CodeStartLine] = "{{{" ~> rest ^^ {
    case directives =>
      val directive_map = if (!directives.isEmpty) {
        directives.split('|') map {pair =>
          val keyval = pair.split(':')
          keyval(0) -> keyval(1)
        } toMap
      } else {
        Map[String,String]()
      }

      CodeStartLine(
        directives = directive_map
      )
  }

  val codeEnd: Parser[CodeEndLine] = "}}}" ^^^ {CodeEndLine()}

  val quoteParser: Parser[QuoteLine] = ">>>\\s?".r ~> rest ^^ {case text => QuoteLine(text)}

  val unorderedListParser: Parser[UnorderedLine] = "\\*\\s?".r ~> rest ^^ {case text => UnorderedLine(text)}
  val orderedListParser: Parser[OrderedLine] = "\\d+\\.\\s?".r ~> rest ^^ {case text => OrderedLine(text)}

}

