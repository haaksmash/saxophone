package com.haaksmash.saxophone.intakes

import org.scalatest._

import java.io.File

class FileIntakeSpec extends FlatSpec {

  val article_filepath = getClass.getResource("/article_example.sax").getPath

  "FileIntake" should "successfully evaluate a .sax file by path" in {
    val document = FileIntake(article_filepath)

    assert(document.isDefined)
    assert(document.get.children.length > 1)
  }

  it should "evaluate a .sax file directly" in {
    val article = new File(article_filepath)
    val document = FileIntake(article)

    assert(document.isDefined)
    assert(document.get.children.length > 1)
  }
}
