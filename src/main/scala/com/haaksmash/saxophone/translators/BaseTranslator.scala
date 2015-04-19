package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone._

trait BaseTranslator {


  /**
   * Fallback translator, blindly returns `node.toString`. If we reach
   * this method, something's gone wrong with the translator.
   * @param node
   * @return String
   */
  def node(node:Node): String = node.toString

  def heading(node:Heading): String
  def paragraph(node:Paragraph): String
  def code(node:Code): String
  def quote(node:Quote): String
  def orderedList(node:OrderedList) : String
  def unorderedList(node:UnorderedList): String
  def footnote(node:Footnote): String
  def link(node:Link): String

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta data about their contents.
   */
  def emphasizedText(node:EmphasizedText): String
  def forcedNewLine(node:ForcedNewline): String
  def standardText(node:StandardText): String
  def struckthroughText(node:StruckthroughText): String
  def underlinedText(node:UnderlinedText): String
  def weightedText(node:WeightedText): String
  def monospacedText(node:MonospaceText): String

  def node_to_translator(n:Node) = n match {
    case n: StandardText => standardText(n)
    case n: Link => link(n)
    case n: EmphasizedText => emphasizedText(n)
    case n: WeightedText => weightedText(n)
    case n: Paragraph => paragraph(n)
    case n: Heading => heading(n)
    case n: UnorderedList => unorderedList(n)
    case n: OrderedList => orderedList(n)
    case n: Code => code(n)
    case n: StruckthroughText => struckthroughText(n)
    case n: UnderlinedText => underlinedText(n)
    case n: MonospaceText => monospacedText(n)
    case n: Quote => quote(n)
    case n => this.node(n)
  }

  def translate(node: Node): String = {

    val output = node.children match {
      case Seq() => Traversable(translateSingle(node))
      case children => children map {node_to_translator(_)}
    }
    output.mkString
  }

  def translateSingle(node:Node): String = {
    node_to_translator(node)
  }
}
