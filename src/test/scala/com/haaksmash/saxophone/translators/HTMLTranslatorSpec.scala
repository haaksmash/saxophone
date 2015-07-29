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

class HTMLTranslatorSpec extends FlatSpec {

  def translator = new HTMLTranslator

  "quote" should "translate a Quote" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), None)

    val result = translator.quote(quote)

    assert(result == "<figure class=\"quote\"><blockquote>Hello, Clarice.</blockquote></figure>")
  }

  it should "translate a Quote with a source" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), Some(Seq(StandardText("Silence of the Lambs"))))

    val result = translator.quote(quote)

    assert(result == "<figure class=\"quote\"><blockquote>Hello, Clarice.</blockquote><figcaption>Silence of the Lambs</figcaption></figure>")
  }

  "heading" should "translate a Heading" in {
    for (level <- 1 to 5) {
      val heading = Heading(level, Seq(StandardText("This is a heading!")))

      val result = translator.heading(heading)

      assert(result == s"""<h$level>This is a heading!</h$level>""")
    }
  }

  "paragraph" should "translate a Paragraph" in {
    val paragraph = Paragraph(Seq(StandardText("This is a paragraph")))

    val result = translator.paragraph(paragraph)

    assert(result == "<p>This is a paragraph</p>")
  }

  "code" should "translate a Code" in {
    val code = Code(Map[String,String](), "this  is code,\n yes")

    val result = translator.code(code)

    assert(result ==
      """<figure class="code"><code>this  is code,
        | yes</code></figure>""".stripMargin)
  }

  it should "escape characters" in {
    val code = Code(Map[String,String](), "x <= y")

    val result = translator.code(code)

    assert(result == "<figure class=\"code\"><code>x &lt;= y</code></figure>")
  }

  it should "handle directives like a boss" in {
    val code = Code(Map("lang" -> "magic"), "square = lambda x: x ** 2\nsquare(2)")

    val result = translator.code(code)

    assert(result ==
      """<figure class="code"><code lang="magic">square = lambda x: x ** 2
        |square(2)</code></figure>""".stripMargin)
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

    assert(result == "<ol><li>Line One</li><li>Line Two</li><li>Line Three</li></ol>")
  }

  it should "emit an 'unordered list' if commanded to do so" in {
    val list = OrderedList(
      Seq(
        Seq(Paragraph(Seq(StandardText("Line One")))),
        Seq(Paragraph(Seq(StandardText("Line Two")))),
        Seq(Paragraph(Seq(StandardText("Line Three"))))
      ),
      present_unordered=true
    )

    val result = translator.orderedList(list)

    assert(result == "<ul><li>Line One</li><li>Line Two</li><li>Line Three</li></ul>")
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
    assert(result startsWith "<ul>")
    assert(result endsWith "</ul>")
    assert(result contains "<li>Line One</li>")
    assert(result contains "<li><em>Line Two</em></li>")
    assert(result contains "<li>Line Three</li>")
  }

  "link" should "translate a Link" in {
    val link = Link(Seq(StandardText("the link!")), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "<a href=\"the target\">the link!</a>")
  }

  it should "recursively translate" in {
    val link = Link(Seq(EmphasizedText("the link!", Map())), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "<a href=\"the target\"><em>the link!</em></a>")
 }

  "emphasizedText" should "translate an EmphasizedText" in {
    val text = EmphasizedText("IMPORTANT!", Map())

    val result = translator.emphasizedText(text)

    assert(result == "<em>IMPORTANT!</em>")
  }

  it should "honor meta" in {
    val text = EmphasizedText("IMPORTANT!", Map("class" -> "red"))

    val result = translator.emphasizedText(text)

    assert(result == "<em class=\"red\">IMPORTANT!</em>")
  }

  it should "escape its text" in {

    val text = EmphasizedText("<look> &<a tag>", Map())
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }

  "weightedText" should "translate a WeightedText" in {
    val text = WeightedText(1, "HEAVY!", Map())

    val result = translator.weightedText(text)

    assert(result == "<strong>HEAVY!</strong>")
  }

  it should "escape its text" in {

    val text = WeightedText(1, "<look> &<a tag>", Map())
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }

  it should "honor meta" in {
    val text = WeightedText(1, "HEAVY!", Map("class" -> "red"))

    val result = translator.weightedText(text)

    assert(result == "<strong class=\"red\">HEAVY!</strong>")
  }

  "standardText" should "translate a StandardText" in {
    val text = StandardText("this is text")

    val result = translator.standardText(text)

    assert(result == "this is text")
  }

  it should "escape its text" in {

    val text = StandardText("<look> &<a tag>")
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }

  "struckthroughText" should "translate a StruckthroughText" in {
    val text = StruckthroughText("mah texticles!", Map())

    val result = translator.struckthroughText(text)

    assert(result == "<s>mah texticles!</s>")
  }

  "markedText" should "translate an MarkedText" in {
    val text = MarkedText("defective link", Map())

    val result = translator.markedText(text)

    assert(result == "<mark>defective link</mark>")
  }

  it should "honor meta" in {
    val text = MarkedText("defective link", Map("class" -> "red"))

    val result = translator.markedText(text)

    assert(result == "<mark class=\"red\">defective link</mark>")
  }

  "rawText" should "translate a RawText" in {
    val text = RawText("<hohoho>")
    val result = translator.rawText(text)

    assert(result == "<hohoho>")
  }

  it should "escape the RawText if told to do so" in {
    val text = RawText("<hohoho>")
    val result = new HTMLTranslator(allow_raw_strings=false).rawText(text)

    assert(result == "&lt;hohoho&gt;")
  }

  it should "escape its text" in {

    val text = MarkedText("<look> &<a tag>", Map())
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }

  "forcedNewline" should "translate a ForcedNewLine" in {
    val text = ForcedNewline()

    val result = translator.forcedNewLine(text)

    assert(result == "<br/>")
  }

  "monospacedText" should "translate a MonospaceText" in {
    val text = MonospaceText("some_codeling", Map())

    val result = translator.monospacedText(text)

    assert(result == "<code>some_codeling</code>")
  }

  it should "escape its text" in {

    val text = MonospaceText("<look> &<a tag>", Map())
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }

  it should "honor meta" in {
    val text = MonospaceText("defective link", Map("class" -> "red"))

    val result = translator.monospacedText(text)

    assert(result == "<code class=\"red\">defective link</code>")
  }

  "footnote" should "translate footnotes" in {
    val text = Footnote(Seq(StandardText("this is a footnote!")))

    val result = translator.footnote(text)

    assert(result == "<a href=\"#note:1\" name=\"rn:1\" rel=\"footnote\">1</a>")
  }

  it should "be emitted at the end of the document" in {
    val text = Document(Seq(Paragraph(Seq(
      StandardText("Greetings, humans"),
      Footnote(Seq(StandardText("humans is here used to refer to any sapient creature"))),
      StandardText("!")
    ))))

    val result = translator.translate(text)

    assert(result == "<p>Greetings, humans<a href=\"#note:1\" name=\"rn:1\" rel=\"footnote\">1</a>!</p><footer><p><a class=\"fnote\" href=\"#rn:1\" name=\"note:1\">1</a> humans is here used to refer to any sapient creature</p></footer>")
  }

  it should "increment its footer numbers, etc" in {
    val text = Document(Seq(Paragraph(Seq(
      StandardText("Greetings,"), Footnote(Seq(StandardText("hohoho"))), StandardText(" humans"),
      Footnote(Seq(StandardText("humans is here used to refer to any sapient creature"))),
      StandardText("!")
    ))))

    val result = translator.translate(text)

    assert(result == "<p>Greetings,<a href=\"#note:1\" name=\"rn:1\" rel=\"footnote\">1</a> humans<a href=\"#note:2\" name=\"rn:2\" rel=\"footnote\">2</a>!</p><footer><p><a class=\"fnote\" href=\"#rn:1\" name=\"note:1\">1</a> hohoho</p><p><a class=\"fnote\" href=\"#rn:2\" name=\"note:2\">2</a> humans is here used to refer to any sapient creature</p></footer>")

  }

  it should "not be emitted at the end of a not-document" in {
    val text = Paragraph(Seq(
      StandardText("Greetings, humans"),
      Footnote(Seq(StandardText("humans is here used to refer to any sapient creature"))),
      StandardText("!")
    ))

    val result = translator.translate(text)

    assert(result == "Greetings, humans<a href=\"#note:1\" name=\"rn:1\" rel=\"footnote\">1</a>!")
  }

  it should "put alt text instead of footer if told to do so" in {
    val text = Document(Seq(Paragraph(Seq(
      StandardText("Greetings, humans"),
      Footnote(Seq(StandardText("humans is here used to refer to any \"sapient\" creature"))),
      StandardText("!")
    ))))

    val result = new HTMLTranslator(footnote_as_title_text=true).translate(text)

    assert(result == "<p>Greetings, humans<a title=\"humans is here used to refer to any \\\"sapient\\\" creature\" rel=\"footnote\">1</a>!</p>")
  }
}
