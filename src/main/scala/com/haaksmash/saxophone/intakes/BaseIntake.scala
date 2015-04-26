package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.primitives.Document

trait BaseIntake {
  type IntakeType
  def intake(input:IntakeType): Option[Document]
}
