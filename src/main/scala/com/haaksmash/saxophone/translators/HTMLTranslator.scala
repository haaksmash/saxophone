package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone._


object HTMLTranslator extends BaseTranslator {

  /*
   * These are block-level nodes; i.e., nodes that should have their
   * children translated recursively.
   */
  def heading(node:Heading) = s"""<h${node.level}>${translate(node)}</h${node.level}>"""
  def paragraph(node:Paragraph) = s"""<delegateParsing>${translate(node)}</delegateParsing>"""
  def code(node:Code) = s"""<pre><code>${node.contents}</code></pre>"""
  def quote(node:Quote) = s"""<blockquote>${translate(node)}</blockquote>"""
  def orderedList(node:OrderedList) = {
    val list_items = node.items.foldLeft("")((s, li) => s + s"""<li>${translate(li)}</li>""")
    s"""<ol>$list_items</ol>"""
  }
  def unorderedList(node:UnorderedList) = {
    val list_items = node.items.foldLeft("")((s, li) => s + s"""<li>${translate(li)}</li>""")
    s"""<ul>${list_items}</ul>"""
  }
  def footnote(node:Footnote) = "FOOTNOTE FUNC"
  def link(node:Link) = s"""<a href="${node.to}">${translate(node)}</a>"""

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta data about their contents.
   */
  def emphasizedText(node:EmphasizedText) = s"<em>${node.text}</em>"
  def forcedNewLine(node:ForcedNewline) = "BR FUNC"
  def standardText(node:StandardText) = node.text
  def struckthroughText(node:StruckthroughText) = s"<s>${node.text}</s>"
  def underlinedText(node:UnderlinedText) = s"""<span style="text-decoration:underline">${node.text}</span>"""
  def weightedText(node:WeightedText) = s"<strong>${node.text}</strong>"

  override def translate(node: Node): String = {

    val html = node.children match {
      case Seq() => Traversable(translateSingle(node))
      case children => children map {node_to_translator(_)}
    }
    html.mkString
  }
}
