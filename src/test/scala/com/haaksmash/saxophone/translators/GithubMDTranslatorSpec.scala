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
        Paragraph(Seq(StandardText("Line One"))),
        Paragraph(Seq(StandardText("Line Two"))),
        Paragraph(Seq(StandardText("Line Three")))
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
        Paragraph(Seq(StandardText("Line One"))),
        // ensures recursive translation
        Paragraph(Seq(EmphasizedText("Line Two"))),
        Paragraph(Seq(StandardText("Line Three")))
      )
    )

    val result = translator.unorderedList(list)

    // We can't assert the actual HTML string output because Seq doesn't make
    // guarantees about the order of the list elements
    assert(result contains "* Line One\n")
    assert(result contains "* *Line Two*\n")
    assert(result contains "* Line Three\n")
  }

  "link" should "translate a Link" in {
    val link = Link(Seq(StandardText("the link!")), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "[the link!](the target)")
  }

  it should "recursively translate" in {
    val link = Link(Seq(EmphasizedText("the link!")), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "[*the link!*](the target)")
  }

  "emphasizedText" should "translate an EmphasizedText" in {
    val text = EmphasizedText("IMPORTANT!")

    val result = translator.emphasizedText(text)

    assert(result == "*IMPORTANT!*")
  }

  "weightedText" should "translate a WeightedText" in {
    val text = WeightedText(1, "HEAVY!")

    val result = translator.weightedText(text)

    assert(result == "**HEAVY!**")
  }

  "standardText" should "translate a StandardText" in {
    val text = StandardText("this is text")

    val result = translator.standardText(text)

    assert(result == "this is text")
  }

  "struckthroughText" should "translate a StruckthroughText" in {
    val text = StruckthroughText("mah texticles!")

    val result = translator.struckthroughText(text)

    assert(result == "~~mah texticles!~~")
  }

  "underlinedText" should "transparently fail" in {
    val text = UnderlinedText("defective link")

    val result = translator.underlinedText(text)

    assert(result == "defective link")
  }

  "forcedNewline" should "translate a ForcedNewLine" in {
    val text = ForcedNewline()

    val result = translator.forcedNewLine(text)

    assert(result == "\n\n")
  }

  "monospacedText" should "translate a MonospaceText" in {
    val text = MonospaceText("some_codeling")

    val result = translator.monospacedText(text)

    assert(result == "`some_codeling`")
  }
}
