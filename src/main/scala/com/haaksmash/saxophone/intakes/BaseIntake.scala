package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.Document

trait BaseIntake {
  def intake(input:String): Document
}
