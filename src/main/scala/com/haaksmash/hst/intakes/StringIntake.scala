package com.haaksmash.hst.intakes

import com.haaksmash.hst.{BlockParser, StringLineTokenizer, LineReader}
import scala.util.parsing.combinator._

class StringIntake(
                    val string_tokenizer: StringLineTokenizer = new StringLineTokenizer,
                    val block_parser: BlockParser = new BlockParser) extends BaseIntake {

  def intake(input: String) = {
    val lines = string_tokenizer.eval(input)
    (lines map {
      l => block_parser.blocks(new LineReader(l))
    }).get.get
  }

}

object StringIntake {
  def apply(input:String) = {
    new StringIntake intake(input)
  }
}
