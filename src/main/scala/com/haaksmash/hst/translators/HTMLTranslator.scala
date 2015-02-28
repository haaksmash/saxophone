package com.haaksmash.hst.translators

import com.haaksmash.hst._


object HTMLTranslator extends BaseTranslator {

  /**
   * Fallback translator, blindly returns `node.toString`. If we reach
   * this method, something's gone wrong with the translator.
   * @param node
   * @return String
   */
  def node(node:Node) = "NODE"

  /*
   * These are block-level nodes; i.e., nodes that should have their
   * children translated recursively.
   */
  def heading(node:Heading) = s"""<h${node.level}>${translate(node)}</h${node.level}>"""
  def paragraph(node:Paragraph) = s"""<p>${translate(node)}</p>"""
  def code(node:Code) = "CODE FUNC"
  def quote(node:Quote) = s"""<blockquote>${translate(node)}</blockquote>"""
  def orderedList(node:OrderedList) = {
    val list_items = node.items.foldLeft("")((s, li) => s + s"""<li>${translate(li)}</li>""")
    s"""<ol>$list_items</ol>"""
  }
  def unorderedList(node:UnorderedList) = {
    val list_items = node.items.foldLeft("")((s, li) => s + s"""<li>${translate(li)}</li>""")
    s"""<ul>${node.items}</ul>"""
  }
  def footnote(node:Footnote) = "FOOTNOTE FUNC"
  def link(node:Link) = s"""<a href="${node.to}">${translate(node)}</a>"""

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta data about their contents.
   */
  def emphasizedText(node:EmphasizedText) = "EM FUNC"
  def forcedNewLine(node:ForcedNewline) = "BR FUNC"
  def standardText(node:StandardText) = node.text
  def struckthroughText(node:StruckthroughText) = "STRIKE FUNC"
  def underlinedText(node:UnderlinedText) = "U FUNC"
  def weightedText(node:WeightedText) = "STRONG FUNC"

  def node_to_translator(n:Node) = n match {
    case n: StandardText => standardText(n)
    case n: Link => link(n)
    case n: Paragraph => paragraph(n)
    case n: Heading => heading(n)
    case n: UnorderedList => unorderedList(n)
    case n: OrderedList => orderedList(n)
    case n: Code => code(n)
    case n: Quote => quote(n)
    case n => this.node(n)
  }


  def translateSingle(node:Node): String = {
    node_to_translator(node)
  }

  override def translate(node: Node): String = {

    val html = node.children match {
      case Seq() => Traversable(translateSingle(node))
      case children => children map {node_to_translator(_)}
    }
    html.mkString
  }
}
