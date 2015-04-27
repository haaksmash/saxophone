package com.haaksmash.saxophone.intakes

import java.io.File

import com.haaksmash.saxophone.primitives.Document

class FileIntake extends BaseIntake {
  type IntakeType = File

  def intake(filename:IntakeType): Option[Document] = {

    val article_source = scala.io.Source.fromFile(filename)
    val the_article = article_source.mkString
    article_source.close()

    StringIntake(the_article)
  }
}

object FileIntake {
  def apply(filename: String) = {
    (new FileIntake).intake(new File(filename))
  }

  def apply(file:File) = {
    (new FileIntake).intake(file)
  }
}