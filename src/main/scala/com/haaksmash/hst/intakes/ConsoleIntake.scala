package com.haaksmash.hst.intakes

import com.haaksmash.hst.Document

object ConsoleIntake extends BaseIntake {

  val file_intake = new FileIntake

  def intake(filename:String): Document = {
    file_intake.intake(filename)
  }

  def apply(filename:String) = intake(filename)

}
