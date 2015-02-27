package com.haaksmash.hst.intakes

import org.scalatest._

trait Article extends SuiteMixin { this: Suite =>
  val article_filename = "/Users/haak/Dropbox/git_projects/personal_site/app/utility/article_example.hs"
  val article = {
    val article_source = scala.io.Source.fromFile(article_filename)
    val the_article = article_source.mkString
    article_source.close()
    the_article
  }
}

class IntakeSmokeSpec extends FlatSpec with Matchers with Article {

  "StringIntake" should "successfully evaluate a string" in {
    val document = StringIntake(article)
  }

  "FileIntake" should "successfully evaluate a .hst file" in {
    val document = FileIntake(article_filename)
  }

  "ConsoleIntake" should "successfully evaluate a .hst file" in {
    val document = ConsoleIntake(article_filename)
  }

}
