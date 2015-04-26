package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone.primitives._
import org.scalatest._

class HTMLTranslatorSpec extends FlatSpec {

  val translator = new HTMLTranslator

  "quote" should "translate a Quote" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), None)

    val result = translator.quote(quote)

    assert(result == "<blockquote>Hello, Clarice.</blockquote>")
  }

  it should "translate a Quote with a source" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), Some(Seq(StandardText("Silence of the Lambs"))))

    val result = translator.quote(quote)

    assert(result == "<blockquote>Hello, Clarice.<footer>Silence of the Lambs</footer></blockquote>")
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
      """<pre><code>this  is code,
        | yes</code></pre>""".stripMargin)
  }

  it should "optionally exclude the <pre> tag" in {
    val code = Code(Map[String,String](), "this  is code,\n yes")

    val result = new HTMLTranslator(wrap_code_with_pre = false).code(code)

    assert(result ==
      """<code>this  is code,
        | yes</code>""".stripMargin)
  }

  it should "escape characters if not using the pre tag" in {
    val code = Code(Map[String,String](), "x <= y")

    val result = new HTMLTranslator(wrap_code_with_pre = false).code(code)

    assert(result == "<code>x &lt;= y</code>")
  }

  it should "not escape characters if using the pre tag" in {
    val code = Code(Map[String,String](), "x <= y")

    val result = new HTMLTranslator(wrap_code_with_pre = true).code(code)

    assert(result == "<pre><code>x <= y</code></pre>")
  }

  it should "handle directives like a boss" in {
    val code = Code(Map("language" -> "magic"), "square = lambda x: x ** 2\nsquare(2)")

    val result = translator.code(code)

    assert(result ==
      """<pre><code language="magic">square = lambda x: x ** 2
        |square(2)</code></pre>""".stripMargin)
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

    assert(result == "<ol><li>Line One</li><li>Line Two</li><li>Line Three</li></ol>")
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
    val link = Link(Seq(EmphasizedText("the link!")), LinkTarget("the target"))

    val result = translator.link(link)

    assert(result == "<a href=\"the target\"><em>the link!</em></a>")
 }

  "emphasizedText" should "translate an EmphasizedText" in {
    val text = EmphasizedText("IMPORTANT!")

    val result = translator.emphasizedText(text)

    assert(result == "<em>IMPORTANT!</em>")
  }

  it should "escape its text" in {

    val text = EmphasizedText("<look> &<a tag>")
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }

  "weightedText" should "translate a WeightedText" in {
    val text = WeightedText(1, "HEAVY!")

    val result = translator.weightedText(text)

    assert(result == "<strong>HEAVY!</strong>")
  }

  it should "escape its text" in {

    val text = WeightedText(1, "<look> &<a tag>")
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
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
    val text = StruckthroughText("mah texticles!")

    val result = translator.struckthroughText(text)

    assert(result == "<s>mah texticles!</s>")
  }

  "underlinedText" should "translate an UnderlinedText" in {
    val text = UnderlinedText("defective link")

    val result = translator.underlinedText(text)

    assert(result == "<mark>defective link</mark>")
  }

  it should "escape its text" in {

    val text = UnderlinedText("<look> &<a tag>")
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
    val text = MonospaceText("some_codeling")

    val result = translator.monospacedText(text)

    assert(result == "<code>some_codeling</code>")
  }

  it should "escape its text" in {

    val text = MonospaceText("<look> &<a tag>")
    val result = translator.translate(text)

    assert(result.contains("&gt;"))
    assert(result.contains("&lt;"))
    assert(result.contains("&amp;"))
  }
}
