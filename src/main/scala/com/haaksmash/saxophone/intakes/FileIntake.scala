package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.Document

class FileIntake(val string_intake:StringIntake = new StringIntake) extends BaseIntake {

  def intake(filename:String): Document = {

    val article_source = scala.io.Source.fromFile(filename)
    val the_article = article_source.mkString
    article_source.close()

    string_intake.intake(the_article)
  }
}

object FileIntake {
  def apply(filename: String) = {
    (new FileIntake).intake(filename)
  }
}