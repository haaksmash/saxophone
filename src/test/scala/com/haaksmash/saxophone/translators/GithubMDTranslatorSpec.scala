/*
 * saxophone - a markup processing program
 * Copyright (C) 2015  Haak Saxberg
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone.primitives._
import org.scalatest._

class GithubMDTranslatorSpec extends FlatSpec {

  val translator = new GithubMDTranslator

  "quote" should "translate a Quote" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), None)

    val result = translator.quote(quote)

    assert(result == "> Hello, Clarice.\n")
  }

  "heading" should "translate a Heading" in {
    for (level <- 1 to 5) {
      val heading = Heading(level, Seq(StandardText("This is a heading!")))

      val result = translator.heading(heading)

      assert(result == s"""${"#" * level} This is a heading!\n""")
    }
  }

  "paragraph" should "translate a Paragraph" in {
    val paragraph = Paragraph(Seq(StandardText("This is a paragraph")))

    val result = translator.paragraph(paragraph)

    assert(result == "This is a paragraph\n\n")
  }

  "code" should "translate a Code" in {
    val code = Code(Map[String,String](), "this  is code,\n yes")

    val result = translator.code(code)

    assert(result ==
      """```
        |this  is code,
        | yes
        |```
        |""".stripMargin)
  }

  it should "use only the 'lang' directive" in {
    val code = Code(Map("lang" -> "magic", "numbers" -> "false"), "square = lambda x: x ** 2\nsquare(2)")

    val result = translator.code(code)

    assert(result ==
      """```magic
        |square = lambda x: x ** 2
        |square(2)
        |```
        |""".stripMargin)
  }

  "orderedList" should "translate an OrderedList" in {
    val list = OrderedList(
      Seq(
        Seq(Paragraph(Seq(StandardText("Line One")))),
        Seq(Paragraph(Seq(StandardText("Line Two")))),
        Seq(Paragraph(Seq(StandardText("Line Three"))))
      )
    )

    val result = translator.orderedList(list)

    assert(result ==
      """1. Line One
        |2. Line Two
        |3. Line Three
        |
        |""".stripMargin)
  }

  "unorderedList" should "translate an UnorderedList" in {
    val list = UnorderedList(
      Set(
        Seq(Paragraph(Seq(StandardText("Line One")))),
        // ensures recursive translation
        Seq(Paragraph(Seq(EmphasizedText("Line Two", Map())))),
        Seq(Paragraph(Seq(StandardText("Line Three"))))
      )
    )

    val result = translator.unorderedList(list)

    // We can't assert the actual HTML string output because Seq doesn't make
    // guarantees about the order of the list elements
    assert(result contains "* Line One\n")
    assert(result contains "* *Line Two*\n")
    assert(result contains "* Line Three\n")
  }

  "embed" should "translate an EmbedNode" in {
    val embed = ImageEmbedNode(Seq("helloooo"), Map())
    val result = translator.embed(embed)

    assert(result == "![](helloooo)")
  }

  it should "include alt text if present" in {
    val embed = ImageEmbedNode(Seq("helloooo"), Map("alt" -> "an image"))
    val result = translator.embed(embed)

    assert(result == "![an image](helloooo)")
  }

  it should "ignore non-image embeds" in {

    val embed = VideoEmbedNode(Seq("helloooo"), Map("alt" -> "an image"))
    val result = translator.embed(embed)

    assert(result == "")
  }

  "link" should "translate a Link" in {
    val link = Link(Seq(StandardText("the link!")), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "[the link!](the target)")
  }

  it should "recursively translate" in {
    val link = Link(Seq(EmphasizedText("the link!", Map())), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "[*the link!*](the target)")
  }

  "emphasizedText" should "translate an EmphasizedText" in {
    val text = EmphasizedText("IMPORTANT!", Map())

    val result = translator.emphasizedText(text)

    assert(result == "*IMPORTANT!*")
  }

  "weightedText" should "translate a WeightedText" in {
    val text = WeightedText(1, "HEAVY!", Map())

    val result = translator.weightedText(text)

    assert(result == "**HEAVY!**")
  }

  "standardText" should "translate a StandardText" in {
    val text = StandardText("this is text")

    val result = translator.standardText(text)

    assert(result == "this is text")
  }

  "struckthroughText" should "translate a StruckthroughText" in {
    val text = StruckthroughText("mah texticles!", Map())

    val result = translator.struckthroughText(text)

    assert(result == "~~mah texticles!~~")
  }

  "markedText" should "transparently fail" in {
    val text = MarkedText("defective link", Map())

    val result = translator.markedText(text)

    assert(result == "defective link")
  }

  "rawText" should "translate a RawText" in {
    val text = RawText("<`ha`/>")
    val result = translator.rawText(text)

    assert(result == "<`ha`/>")
  }

  "forcedNewline" should "translate a ForcedNewLine" in {
    val text = ForcedNewline()

    val result = translator.forcedNewLine(text)

    assert(result == "\n\n")
  }

  "monospacedText" should "translate a MonospaceText" in {
    val text = MonospaceText("some_codeling", Map())

    val result = translator.monospacedText(text)

    assert(result == "`some_codeling`")
  }
}
