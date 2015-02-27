package com.haaksmash.hst.intakes

import com.haaksmash.hst.emitters.ConsoleEmitter

object ConsoleIntake {

  val file_intake = new FileIntake

  def apply(filename:String) = {
    file_intake(filename)
  }

}
