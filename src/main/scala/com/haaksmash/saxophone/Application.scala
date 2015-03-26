package com.haaksmash.saxophone

import com.haaksmash.saxophone.emitters.ConsoleEmitter
import com.haaksmash.saxophone.intakes.ConsoleIntake
import com.haaksmash.saxophone.translators.{HTMLTranslator, HSTTreeStringTranslator}

class Application {

}

object Application {
  /**
   * @param args should only be a single path-to-file-name. All other passed-in things are ignored.
   */
  def main(args: Array[String]): Unit = {
    val saxophone_filename = args(0)
    val document = ConsoleIntake(saxophone_filename)
    ConsoleEmitter.emit(HSTTreeStringTranslator.translate(document))
    ConsoleEmitter.emit(HTMLTranslator.translate(document))
  }
}
