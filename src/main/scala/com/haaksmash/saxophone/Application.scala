package com.haaksmash.saxophone

import com.haaksmash.saxophone.emitters.{FileEmitter, ConsoleEmitter}
import com.haaksmash.saxophone.intakes.FileIntake
import com.haaksmash.saxophone.translators.{HTMLTranslator, SaxophoneTreeStringTranslator}

class Application {

}

object Application {

  val help_string = """usage: saxophone [-d] [-o filename] input_filename"""

  /**
   * @param args [-d] [-o filename] input_filename
   */
  def main(args: Array[String]): Unit = {

    if (args.contains("--help")) {
      println(help_string)
      return
    }

    val saxophone_filename = args(args.length - 1)

    val document = FileIntake(saxophone_filename)

    if (args.contains("-o")) {
      val output_filename = args(args.indexOf("-o") + 1)
      FileEmitter(output_filename).emit(HTMLTranslator.translate(document))
    } else {
      ConsoleEmitter.emit(HTMLTranslator.translate(document))
    }

    if (args.contains("-d"))
      ConsoleEmitter.emit(SaxophoneTreeStringTranslator.translate(document))
  }
}
