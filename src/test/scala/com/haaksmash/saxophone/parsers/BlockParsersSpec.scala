package com.haaksmash.saxophone.parsers

import com.haaksmash.saxophone.primitives._
import com.haaksmash.saxophone.readers.LineReader
import org.scalatest._

class BlockParsersSpec extends FlatSpec{
  object parsers extends BlockParsers

  "quote_node" should "recognize a QuoteLine" in {
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

  "quote_source" should "recognize inline text within []" in {
    val source = TextLine("[this is a source]")

    val result = parsers.quote_source(new LineReader(Seq(source))).get

    assert(result == Seq(StandardText("this is a source")))
  }

  "code_node" should "grab stuff between CodeStart/CodeEndLine, join them by newlines" in {
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
    val codez = Seq(CodeStartLine(Map("language" -> "scala")), EmptyLine(), CodeEndLine())

    val result = parsers.code_node(new LineReader(codez)).get

    assert(result.directives.getOrElse("language", "not scala") == "scala")
  }

  "uordered_list_node" should "recognize UnorderedLines" in {
    val list = Seq(UnorderedLine("*", "list item A"), UnorderedLine("*", "list item B"))

    val result = parsers.unordered_list_node(new LineReader(list)).get

    assert(result.items == Set(StandardText("list item A"), StandardText("list item B")))
  }

  it should "scoop TextLines into the previous UnorderedLine" in {
    val list = Seq(UnorderedLine("*", "list item A"), TextLine("item A continued"), UnorderedLine("*", "list item B"))

    val result = parsers.unordered_list_node(new LineReader(list)).get

    assert(result.items == Set(StandardText("list item A item A continued"), StandardText("list item B")))
  }

  "ordered_list_node" should "recognize OrderedLines" in {
    val list = Seq(OrderedLine("1.", "list item 1"), OrderedLine("2.", "list item 2"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.items == Seq(StandardText("list item 1"), StandardText("list item 2")))
  }

  it should "scoop TextLines into the previous UnorderedLine" in {
    val list = Seq(OrderedLine("1.", "list item 1"), TextLine("item 1 continued"), OrderedLine("2.", "list item 2"))

    val result = parsers.ordered_list_node(new LineReader(list)).get

    assert(result.items == Seq(StandardText("list item 1 item 1 continued"), StandardText("list item 2")))
  }
}
