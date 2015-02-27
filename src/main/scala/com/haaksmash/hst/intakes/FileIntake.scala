package com.haaksmash.hst.intakes

object FileIntake {
  def apply(filename:String) = {

    val article_source = scala.io.Source.fromFile(filename)
    val the_article = article_source.mkString
    article_source.close()

    StringIntake(the_article)
  }
}
