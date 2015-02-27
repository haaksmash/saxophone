package com.haaksmash.hst.intakes

import com.haaksmash.hst.Document

class ConsoleIntake(val file_intake:FileIntake = new FileIntake) extends BaseIntake {

  def intake(filename:String): Document = {
    file_intake.intake(filename)
  }

}

object ConsoleIntake {
  def apply(filename:String) = {
    new ConsoleIntake intake(filename)
  }
}
