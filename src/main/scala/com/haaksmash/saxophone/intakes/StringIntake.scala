package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone._

class StringIntake(
  val string_tokenizer: LineParsers,
  val block_parser: BlockParsers) extends BaseIntake {

  type IntakeType = String;

  def intake(input: IntakeType): Option[Document] = {
    /*val lines = string_tokenizer.eval(input)
    (lines map {
      l => block_parser.blocks(new LineReader(l))
    }).get.get
    */
    val lines = string_tokenizer.lines(new StringLineReader(input))
    if (lines.isEmpty) {
      return None
    }

    val blocks = block_parser.blocks(new LineReader(lines.get))

    if (blocks.isEmpty) {
      return None
    }

    Some(blocks.get)
  }
}

object StringIntake {
  def apply(input:String): Option[Document] = {
    new StringIntake(
      string_tokenizer = new LineParsers,
      block_parser = new BlockParsers
    ).intake(input)
  }
}
