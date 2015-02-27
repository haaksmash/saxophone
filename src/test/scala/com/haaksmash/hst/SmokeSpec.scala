package com.haaksmash.hst

import com.haaksmash.hst.intakes.{ConsoleIntake, FileIntake, StringIntake}
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
    val parse_result = StringIntake(article)
    parse_result.isDefined shouldBe(true)

    val nodes = parse_result.get
    nodes.successful shouldBe(true)
    nodes.next.atEnd shouldBe(true)
  }

  "FileIntake" should "successfully evaluate a .hst file" in {
    val parse_result = FileIntake(article_filename)
    parse_result.isDefined shouldBe(true)

    val nodes = parse_result.get
    nodes.successful shouldBe(true)
    nodes.next.atEnd shouldBe(true)
  }

  "ConsoleIntake" should "successfully evaluate a .hst file" in {
    val parse_result = ConsoleIntake(article_filename)
    parse_result.isDefined shouldBe(true)

    val nodes = parse_result.get
    nodes.successful shouldBe(true)
    nodes.next.atEnd shouldBe(true)
  }

}
