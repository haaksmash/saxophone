package com.haaksmash.hst.emitters

import com.haaksmash.hst.{Document, Node, BlockParser}

object ConsoleEmitter extends BaseEmitter {
  override def emit(contents: String): Unit = println(contents)
}
