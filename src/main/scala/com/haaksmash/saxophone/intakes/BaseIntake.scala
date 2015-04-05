package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.Document

trait BaseIntake {
  type IntakeType
  def intake(input:IntakeType): Option[Document]
}
