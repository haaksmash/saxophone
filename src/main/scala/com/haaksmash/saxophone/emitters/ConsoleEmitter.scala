package com.haaksmash.saxophone.emitters

import com.haaksmash.saxophone.{Document, Node, BlockParser}

object ConsoleEmitter extends BaseEmitter {
  override def emit(contents: String): Unit = println(contents)
}
