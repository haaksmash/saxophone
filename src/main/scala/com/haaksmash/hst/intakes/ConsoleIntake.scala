package com.haaksmash.hst.intakes

import com.haaksmash.hst.emitters.ConsoleEmitter

object ConsoleIntake {

  /**
   *
   * @param args should only be a single path-to-file-name. All other passed-in things are ignored.
   */
  def main(args: Array[String]): Unit = {
    val filename = args(0)
    FileIntake(filename) map {ConsoleEmitter(_)}
  }

}
