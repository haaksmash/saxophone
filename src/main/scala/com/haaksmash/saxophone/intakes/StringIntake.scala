package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.{Document, BlockParsers, LineReader, LineParsers}

class StringIntake(
  val string_tokenizer: LineParsers,
  val block_parser: BlockParsers) extends BaseIntake {

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
      string_tokenizer = new LineParsers,
      block_parser = new BlockParsers
    ).intake(input)
  }
}
