package com.haaksmash.hst

import com.haaksmash.hst.intakes.{FileIntake, StringIntake}
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

class SmokeSpec extends FlatSpec with Matchers with Article {

  "StringIntake" should "successfully evaluate a string" in {
    StringIntake(article) map { r => println(r.get) }
  }
  "FileIntake" should "successfully evaluate a .hst file" in {
    FileIntake(article_filename) map { r => println(r.get) }
  }

}
