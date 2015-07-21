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

package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.primitives._
import com.haaksmash.saxophone.readers.LineReader
import org.scalatest._

class BlockParsersSpec extends FlatSpec {
  object parsers extends BlockParsers

  "quote_node" must "recognize a QuoteLine" in {
    val quote_line = QuoteLine("this is a quote")

    val result = parsers.quote_node(new LineReader(Seq(quote_line))).get

    assert(result.children == Seq(StandardText("this is a quote")))
  }

  it should "compress multiple QuoteLines" in {
    val quote_lines = Seq(
      QuoteLine("this is a quote"),
      QuoteLine("and this is the same quote")
    )

    val result = parsers.quote_node(new LineReader(quote_lines)).get

    assert(result.children == Seq(StandardText("this is a quote and this is the same quote")))
  }

  it should "handle a quote with a source" in {
    val quote_lines = Seq(
      QuoteLine("this is a quote"),
      TextLine("[a source]")
    )

    val result = parsers.quote_node(new LineReader(quote_lines)).get

    assert(result.source.get == Seq(StandardText("a source")))
  }

  "quote_source" must "recognize inline text within []" in {
    val source = TextLine("[this is a source]")

    val result = parsers.quote_source(new LineReader(Seq(source))).get

    assert(result == Seq(StandardText("this is a source")))
  }

  "code_node" must "grab stuff between CodeStart/CodeEndLine, join them by newlines" in {
    val codez = Seq(CodeStartLine(Map()), TextLine("line one"), TextLine("/line two/"), UnorderedLine("*", "a line a line"), CodeEndLine())

    val result = parsers.code_node(new LineReader(codez)).get

    assert(result.contents == "line one\n/line two/\n* a line a line")
    assert(result.directives == Map())
  }

  it should "require something between CodeStart/CodeEndLine" in {
    val codez = Seq(CodeStartLine(Map()), CodeEndLine())

    val result = parsers.code_node(new LineReader(codez))

    assert(result.isEmpty)
  }

  it should "propagate directives" in {
    val codez = Seq(CodeStartLine(Map("lang" -> "scala")), EmptyLine(), CodeEndLine())

    val result = parsers.code_node(new LineReader(codez)).get

    assert(result.directives.getOrElse("lang", "not scala") == "scala")
  }

  "unordered_list_node" must "recognize UnorderedLines" in {
    val list = Seq(UnorderedLine("*", "list item A"), UnorderedLine("*", "list item B"))

    val result = parsers.unordered_list_node(new LineReader(list)).get

    assert(result.items == Set(Seq(Paragraph(Seq(StandardText("list item A")))), Seq(Paragraph(Seq(StandardText("list item B"))))))
  }

  it should "recursively parse its leading line" in {
    val list = Seq(UnorderedLine("*", "list /item/ A"))

    val result = parsers.unordered_list_node(new LineReader(list)).get

    assert(result.items == Set(
      Seq(Paragraph(Seq(StandardText("list "), EmphasizedText("item"), StandardText(" A"))))
    ))
  }

  it should "scoop TextLines into the previous UnorderedLine" in {
    val list = Seq(UnorderedLine("*", "list item A"), TextLine("item A continued"), UnorderedLine("*", "list item B"))

    val result = parsers.unordered_list_node(new LineReader(list)).get

    assert(result.items == Set(Seq(Paragraph(Seq(StandardText("list item A item A continued")))), Seq(Paragraph(Seq(StandardText("list item B"))))))
  }

  it should "not scoop nonTextLines into the previous UnorderedLine" in {
    val list = Seq(UnorderedLine("*", "list item A"), UnorderedLine("*", "list item B"), QuoteLine("item B NOT continued"))

    val result = parsers.unordered_list_node(new LineReader(list)).get

    assert(result.items == Set(Seq(Paragraph(Seq(StandardText("list item A")))), Seq(Paragraph(Seq(StandardText("list item B"))))))
  }

  "ordered_list_node" must "recognize OrderedLines" in {
    val list = Seq(OrderedLine("1.", "list item 1"), OrderedLine("2.", "list item 2"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.items == Seq(Seq(Paragraph(Seq(StandardText("list item 1")))), Seq(Paragraph(Seq(StandardText("list item 2"))))))
  }

  it should "realize that it is meant to appear unordered" in {
    val list = Seq(OrderedLine("-", "list item 1"), OrderedLine("-", "list item 2"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.present_unordered == true)
  }

  it should "recursively parse its leading line" in {
    val list = Seq(OrderedLine("1.", "list /item/ A"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.items == Seq(
      Seq(Paragraph(Seq(StandardText("list "), EmphasizedText("item"), StandardText(" A"))))
    ))
  }

  it should "scoop TextLines into the previous OrderedLine" in {
    val list = Seq(OrderedLine("1.", "list item 1"), TextLine("item 1 continued"), OrderedLine("2.", "list item 2"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.items == Seq(Seq(Paragraph(Seq(StandardText("list item 1 item 1 continued")))), Seq(Paragraph(Seq(StandardText("list item 2"))))))
  }

  it should "not scoop nonTextLines into the previous OrderedLine" in {
    val list = Seq(OrderedLine("1.", "list item 1"), OrderedLine("2.", "list item 2"), QuoteLine("item 2 NOT continued"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.items == Seq(Seq(Paragraph(Seq(StandardText("list item 1")))), Seq(Paragraph(Seq(StandardText("list item 2"))))))
  }

  "paragraph" must "eat as many TextLines as it wants" in {
    val text = Seq(
      TextLine("line one"),
      TextLine("line two"),
      TextLine("line three"),
      TextLine("line four")
    )

    val result = parsers.paragraph(new LineReader(text)).get

    assert(result.children == Seq(StandardText("line one line two line three line four")))
  }

  it must "require at least one TextLine" in {
    val text = Seq()

    val result = parsers.paragraph(new LineReader(text))

    assert(result.isEmpty)
  }

  it should "recursively parse its contents" in {
    val text = Seq(
      TextLine("`mono`"),
      TextLine("*weight*"),
      TextLine("/emph/"),
      TextLine("_mark_")
    )

    val result = parsers.paragraph(new LineReader(text)).get

    assert(result.children == Seq(
      MonospaceText("mono"), StandardText(" "),
      WeightedText(1, "weight"), StandardText(" "),
      EmphasizedText("emph"), StandardText(" "),
      UnderlinedText("mark")
    ))
  }

  "header_node" must "recognize HeaderLines" in {
    val text = HeadingLine("#", "a header!")

    val result = parsers.header_node(new LineReader(Seq(text))).get

    assert(result.level == 1)
    assert(result.children == Seq(StandardText("a header!")))
  }

  it should "propagate level information" in {
    val text = HeadingLine("###", "header!")
    val result = parsers.header_node(new LineReader(Seq(text))).get
    assert(result.level == 3)
  }

  it should "not parse its inline children" in {
    val text = HeadingLine("#", "*strong header*!")
    val result = parsers.header_node(new LineReader(Seq(text))).get
    assert(result.children == Seq(StandardText("*strong header*!")))
  }

  "line" must "match the provided Line class" in {
    for (c <- Seq(
      HeadingLine("#", "text"),
      TextLine("text"),
      UnorderedLine("*", "text"),
      CodeStartLine(Map())
    )) {
      val result = parsers.line(c.getClass)(new LineReader(Seq(c)))
      assert(!result.isEmpty)
    }
  }

  it should "not match a not-provided Line class" in {
    val lines = Set(
      HeadingLine("#", "text"),
      TextLine("text"),
      UnorderedLine("*", "text"),
      CodeStartLine(Map())
    )
    for (c <- lines; o <- lines - c) {
      val result = parsers.line(c.getClass)(new LineReader(Seq(o)))
      assert(result.isEmpty)
    }
  }

  "notLine" must "not match the provided Line class" in {
    for (c <- Seq(
      HeadingLine("#", "text"),
      TextLine("text"),
      UnorderedLine("*", "text"),
      CodeStartLine(Map())
    )) {
      val result = parsers.notLine(c.getClass)(new LineReader(Seq(c)))
      assert(result.isEmpty)
    }
  }

  it should "match a not-provided Line class" in {
    val lines = Set(
      HeadingLine("#", "text"),
      TextLine("text"),
      UnorderedLine("*", "text"),
      CodeStartLine(Map())
    )
    for (c <- lines; o <- lines - c) {
      val result = parsers.notLine(c.getClass)(new LineReader(Seq(o)))
      assert(!result.isEmpty)
    }
  }
}
