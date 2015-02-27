package com.haaksmash.hst.intakes

import com.haaksmash.hst.{BlockParser, StringLineTokenizer, LineReader}

class StringIntake {

  val string_tokenizer = new StringLineTokenizer
  val block_parser = new BlockParser
  
  def eval(input: String)  = {
    val lines = string_tokenizer.eval(input)
    lines map {
      l => block_parser.blocks(new LineReader(l))
    }
  }

}

object StringIntake {
  def apply(input:String) = {
    new StringIntake eval(input)
  }
}
