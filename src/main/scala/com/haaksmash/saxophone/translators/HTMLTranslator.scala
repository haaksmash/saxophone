package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone._

import scala.collection.immutable.ListMap


class HTMLTranslator(wrap_code_with_pre: Boolean = true) extends BaseTranslator {

  /*
   * These are block-level nodes; i.e., nodes that should have their
   * children translated recursively.
   */
  def heading(node:Heading) = s"""<h${node.level}>${translate(node)}</h${node.level}>"""
  def paragraph(node:Paragraph) = s"""<p>${translate(node)}</p>"""
  def code(node:Code) = {
    val code_block = s"""<code${node.directives.foldLeft(""){case (s, (k, v)) => s"""$s $k="$v""""}}>${node.contents}</code>"""
    if (wrap_code_with_pre)
      s"""<pre>$code_block</pre>"""
    else
      escapeTextForHTML(code_block)
  }
  def quote(node:Quote) = {
    val quote = s"""<blockquote>${translate(node)}</blockquote>"""
    if (node.source.isDefined)
      quote + (s"""<div class="source">${node.source.get.map(translate(_)).mkString}</div>""")
    else
      quote
  }
  def orderedList(node:OrderedList) = {
    val list_items = node.items.foldLeft("")((s, li) => s + s"""<li>${translate(li)}</li>""")
    s"""<ol>$list_items</ol>"""
  }
  def unorderedList(node:UnorderedList) = {
    val list_items = node.items.foldLeft("")((s, li) => s + s"""<li>${translate(li)}</li>""")
    s"""<ul>${list_items}</ul>"""
  }

  def footnote(node:Footnote) = "" // footnote isn't supported yet, sorry!

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * metadata about their contents.
   */
  def link(node:Link) = s"""<a href="${node.to}">${translate(node)}</a>"""
  def emphasizedText(node:EmphasizedText) = s"<em>${escapeTextForHTML(node.text)}</em>"
  def forcedNewLine(node:ForcedNewline) = "<br/>"
  def standardText(node:StandardText) = escapeTextForHTML(node.text)
  def struckthroughText(node:StruckthroughText) = s"<s>${escapeTextForHTML(node.text)}</s>"
  def underlinedText(node:UnderlinedText) = s"""<mark>${escapeTextForHTML(node.text)}</mark>"""
  def weightedText(node:WeightedText) = s"<strong>${escapeTextForHTML(node.text)}</strong>"
  def monospacedText(node:MonospaceText) = s"<code>${escapeTextForHTML(node.text)}</code>"

  /**
   * Escapes a string so that it's safe for HTML; e.g., replacing {@code <} with {@code &amp;lt;}.
   * @param text the string that needs escaping
   * @return
   */
  private def escapeTextForHTML(text:String):String = {
    // Order is important here, so we use a ListMap to preserve it.
    val chars_to_escape_sequence = ListMap(
      "&" -> "&amp;", // must be first so we don't accidentally kill anything in the below
      "<" -> "&lt;",
      ">" -> "&gt;",
      "---" -> "&mdash;", // but really you should just use the correct character, —.
      "--" -> "&ndash;" // same here; just use the en-dash character directly.
    )

    var new_text = text
    for (char <- chars_to_escape_sequence.keys){
      new_text = new_text.replaceAll(char, chars_to_escape_sequence(char))
    }
    new_text
  }

}

object HTMLTranslator {
  def translate(node:Node): String = {
    (new HTMLTranslator(true)).translate(node)
  }
}
