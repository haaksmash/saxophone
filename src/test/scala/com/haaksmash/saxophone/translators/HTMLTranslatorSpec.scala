package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone._
import org.scalatest._

class HTMLTranslatorSpec extends FlatSpec {

  val translator = new HTMLTranslator

  "HTMLTranslator.quote" should "translate a Quote" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), None)

    val result = translator.quote(quote)

    assert(result == "<blockquote>Hello, Clarice.</blockquote>")
  }

  it should "translate a Quote with a source" in {
    val quote = Quote(Seq(StandardText("Hello, Clarice.")), Some(Seq(StandardText("Silence of the Lambs"))))

    val result = translator.quote(quote)

    assert(result == "<blockquote>Hello, Clarice.</blockquote><div class=\"source\">Silence of the Lambs</div>")
  }

  "HTMLTranslator.heading" should "translate a Heading" in {
    for (level <- 1 to 5) {
      val heading = Heading(level, Seq(StandardText("This is a heading!")))

      val result = translator.heading(heading)

      assert(result == s"""<h$level>This is a heading!</h$level>""")
    }
  }

  "HTMLTranslator.paragraph" should "translate a Paragraph" in {
    val paragraph = Paragraph(Seq(StandardText("This is a paragraph")))

    val result = translator.paragraph(paragraph)

    assert(result == "<p>This is a paragraph</p>")
  }

  "HTMLTranslator.code" should "translate a Code" in {
    val code = Code(Map[String,String](), "this  is code,\n yes")

    val result = translator.code(code)

    assert(result ==
      """<pre><code>this  is code,
        | yes</code></pre>""".stripMargin)
  }

  it should "optionally exclude the <pre> tag" in {
    val code = Code(Map[String,String](), "this  is code,\n yes")

    val result = (new HTMLTranslator(wrap_code_with_pre = false)).code(code)

    assert(result ==
      """<code>this  is code,
        | yes</code>""".stripMargin)
  }

  it should "handle directives like a boss" in {
    val code = Code(Map("language" -> "magic"), "square = lambda x: x ** 2\nsquare(2)")

    val result = translator.code(code)

    assert(result ==
      """<pre><code language="magic">square = lambda x: x ** 2
        |square(2)</code></pre>""".stripMargin)
  }

}
