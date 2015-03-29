package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.{Document, BlockParser, LineReader, StringLineTokenizer}

class StringIntake(
  val string_tokenizer: StringLineTokenizer,
  val block_parser: BlockParser) extends BaseIntake {

  type IntakeType = String;

  def intake(input: IntakeType): Document = {
    val lines = string_tokenizer.eval(input)
    (lines map {
      l => block_parser.blocks(new LineReader(l))
    }).get.get
  }

}

object StringIntake {
  def apply(input:String): Document = {
    new StringIntake(
      string_tokenizer = new StringLineTokenizer,
      block_parser = new BlockParser
    ).intake(input)
  }
}
