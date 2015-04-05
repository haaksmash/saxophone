package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.Document


object ConsoleIntake {
  def apply(): Option[Document] = {
    val lines = io.Source.stdin.getLines().mkString("\n")
    StringIntake(lines)
  }
}
