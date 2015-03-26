package com.haaksmash.saxophone.emitters

import java.io.{BufferedWriter, File, FileWriter}

class FileEmitter(val filename:String) extends BaseEmitter {
  override def emit(contents: String): Unit = {
    val file = new File(filename)
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write(contents)
    writer.close()
  }
}

object FileEmitter {
  def apply(filename:String) = new FileEmitter(filename)
}
