package com.haaksmash.hst

import com.haaksmash.hst.emitters.ConsoleEmitter
import com.haaksmash.hst.intakes.ConsoleIntake
import com.haaksmash.hst.translators.{HTMLTranslator, HSTTreeStringTranslator}

class Application {

}

object Application {
  /**
   * @param args should only be a single path-to-file-name. All other passed-in things are ignored.
   */
  def main(args: Array[String]): Unit = {
    val filename = args(0)
    val document = ConsoleIntake(filename)
    println("Raw document representation")
    ConsoleEmitter.emit(HSTTreeStringTranslator.translate(document))
    println("HTML representation")
    ConsoleEmitter.emit(HTMLTranslator.translate(document))
  }
}
