package com.haaksmash.hst

import com.haaksmash.hst.intakes.StringIntake
import org.scalatest._

trait Article extends SuiteMixin { this: Suite =>
  val article = {
    val article_source = scala.io.Source.fromFile("/Users/haak/Dropbox/git_projects/personal_site/app/utility/article_example.hs")
    val the_article = article_source.mkString
    article_source.close()
    the_article
  }
}

class SmokeSpec extends FlatSpec with Matchers with Article {

  "BlockParsers" should "successfully evaluate a .hst file" in {
    StringIntake(article) map { r => println(r.get) }
  }

}
