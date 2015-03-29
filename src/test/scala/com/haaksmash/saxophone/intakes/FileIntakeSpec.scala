package com.haaksmash.saxophone.intakes

import org.scalatest._

class FileIntakeSpec extends FlatSpec {

  val article_filename = "/Users/haak/Dropbox/git_projects/personal_site/app/utility/article_example.hs"

  "FileIntake" should "successfully evaluate a .saxophone file" in {
    val document = FileIntake(article_filename)
  }
}
