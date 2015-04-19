package com.haaksmash.saxophone


/**
 * Translates [[String]] to specific instances of [[com.haaksmash.saxophone.Line]]
 */
trait StringLineParsers extends UtilParsers {

  val HEADING_GLYPH = "#"
  val CODE_START = "{{{"
  val CODE_END = "}}}"
  val QUOTE_LINE = ">>>"

  val headingParser: Parser[HeadingLine] = s"${HEADING_GLYPH}+ ".r ~ rest ^^ {
    case glyphs ~ text =>
      // glyphs will end with a space that we want to throw away
      HeadingLine(glyphs.slice(0, glyphs.length - 1), text)
  }

  val codeStart: Parser[CodeStartLine] = CODE_START ~> rest ^^ {
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

  val codeEnd: Parser[CodeEndLine] = CODE_END ^^^ {CodeEndLine()}

  val quoteParser: Parser[QuoteLine] = s"$QUOTE_LINE\\s?".r ~ rest ^^ {case leader ~ text => QuoteLine(leader + text)}

  val unorderedListParser: Parser[UnorderedLine] = "\\*\\s?".r ~ rest ^^ {case leader ~ text => UnorderedLine(leader, text)}
  val orderedListParser: Parser[OrderedLine] = "\\d+\\.\\s?".r ~ rest ^^ {case leader ~ text => OrderedLine(leader, text)}

}

