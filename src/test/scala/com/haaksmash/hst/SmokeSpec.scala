package com.haaksmash.hst

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
    val block_parser = new BlockParser()
    block_parser.eval(article) map { r => println(r.get) }
  }

}
