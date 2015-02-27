package com.haaksmash.hst.intakes

import com.haaksmash.hst.Document

trait BaseIntake {
  def intake(input:String): Document
}
