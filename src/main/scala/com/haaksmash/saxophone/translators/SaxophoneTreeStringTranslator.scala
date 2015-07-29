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

package com.haaksmash.saxophone.translators

import com.haaksmash.saxophone.primitives._


class SaxophoneTreeStringTranslator extends NodeTranslator {

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

  override def rawText(node: RawText): String = ???

  override def monospacedText(node: MonospaceText): String = ???

  override def translate(document: Node) = {
    document.toString
  }

}
