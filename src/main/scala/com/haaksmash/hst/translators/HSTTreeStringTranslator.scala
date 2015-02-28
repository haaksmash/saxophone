package com.haaksmash.hst.translators

import com.haaksmash.hst._

object HSTTreeStringTranslator extends BaseTranslator {

  override def heading(node: Heading): String = ???

  override def paragraph(node: Paragraph): String = ???

  override def footnote(node: Footnote): String = ???

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta data about their contents.
   */
  override def emphasizedText(node: EmphasizedText): String = ???

  override def standardText(node: StandardText): String = ???

  override def forcedNewLine(node: ForcedNewline): String = ???

  override def unorderedList(node: UnorderedList): String = ???

  override def orderedList(node: OrderedList): String = ???

  override def weightedText(node: WeightedText): String = ???

  override def underlinedText(node: UnderlinedText): String = ???

  override def struckthroughText(node: StruckthroughText): String = ???

  override def link(node: Link): String = ???

  override def quote(node: Quote): String = ???

  override def code(node: Code): String = ???

  override def translate(document: Node) = {
    document.toString
  }
}
