package com.haaksmash.saxophone


/**
 * StringLineParsers translates String -> Line
 */
trait StringLineParsers extends UtilParsers {
  val headingParser: Parser[HeadingLine] = "#+ ".r ~ rest ^^ {
    case glyphs ~ text => HeadingLine(glyphs, text)
  }

  val codeStart: Parser[CodeStartLine] = "\\{\\{\\{".r ~ rest ^^ {
    case _ ~ directives => CodeStartLine(
      directives.split('|') map {pair =>
        val keyval = pair.split(':');
        keyval(0) -> keyval(1)
      } toMap
    )
  }

  val codeEnd: Parser[CodeEndLine] = "}}}" ^^^ {CodeEndLine()}

  val quoteParser: Parser[QuoteLine] = ">>>" ~ rest ^^ {case _ ~ text => QuoteLine(text)}

  val unorderedListParser: Parser[UnorderedLine] = "*" ~ rest ^^ {case _ ~ text => UnorderedLine(text)}
  val orderedListParser: Parser[OrderedLine] = "\\d+\\.".r ~ rest ^^ {case _ ~ text => OrderedLine(text)}

}

