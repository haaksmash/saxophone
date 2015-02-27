package com.haaksmash.hst.intakes

class FileIntake(val string_intake:StringIntake = new StringIntake) {

  def apply(filename:String) = {

    val article_source = scala.io.Source.fromFile(filename)
    val the_article = article_source.mkString
    article_source.close()

    string_intake.eval(the_article)
  }
}

object FileIntake {
  def apply(filename: String) = {
    (new FileIntake)(filename)
  }
}