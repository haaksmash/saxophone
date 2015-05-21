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

class GithubMDTranslator extends BaseTranslator {
  override def heading(node: Heading): String = s"${"#" * node.level} ${translate(node)}\n"

  override def footnote(node: Footnote): String = ""

  override def paragraph(node: Paragraph): String = {
    s"${translate(node)}\n\n"
  }

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta data about their contents.
   */
  override def emphasizedText(node: EmphasizedText): String = s"*${node.text}*"

  override def standardText(node: StandardText): String = node.text

  override def forcedNewLine(node: ForcedNewline): String = "\n\n"

  override def rawText(node: RawText): String = node.text

  override def unorderedList(node: UnorderedList): String = {
    ((for (line <- node.items) yield s"* ${line.map(translate(_)).mkString}") mkString "\n") + "\n\n"
  }

  override def orderedList(node: OrderedList): String = {
    ((for (line <- node.items) yield s"${node.items.indexOf(line) + 1}. ${line.map(translate(_)).mkString}") mkString "\n") + "\n\n"
  }

  override def weightedText(node: WeightedText): String = s"**${node.text}**"

  override def underlinedText(node: UnderlinedText): String = s"${node.text}"

  override def struckthroughText(node: StruckthroughText): String = s"~~${node.text}~~"

  override def monospacedText(node: MonospaceText): String = {
    s"`${node.text}`"
  }

  override def link(node: Link): String = s"[${translate(node)}](${node.to.target})"

  override def quote(node: Quote): String = s"> ${translate(node)}\n"

  override def code(node: Code): String = s"```${node.directives.getOrElse("lang", "")}\n${node.contents}\n```\n"
}

object GithubMDTranslator {
  def translate(node:Node): String = {
    (new GithubMDTranslator).translate(node)
  }
}
