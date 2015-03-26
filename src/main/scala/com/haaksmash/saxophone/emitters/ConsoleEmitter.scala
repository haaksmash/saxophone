package com.haaksmash.saxophone.emitters

object ConsoleEmitter extends BaseEmitter {
  override def emit(contents: String): Unit = println(contents)
}
