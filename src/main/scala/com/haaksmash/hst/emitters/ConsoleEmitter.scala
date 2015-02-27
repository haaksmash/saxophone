package com.haaksmash.hst.emitters

import com.haaksmash.hst.{Node, BlockParser}

object ConsoleEmitter {
  def apply(parse_result: BlockParser#ParseResult[Node]) = {
    println(parse_result.get)
  }
}
