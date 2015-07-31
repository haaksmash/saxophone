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

class GithubMDTranslator extends NodeTranslator {
  def heading(node: Heading): String = s"${"#" * node.level} ${translate(node)}\n"

  def footnote(node: Footnote): String = ""

  def paragraph(node: Paragraph): String = {
    s"${translate(node)}\n\n"
  }

  /*
   * Inline nodes; i.e., nodes that don't have children, but only capture
   * meta data about their contents.
   */
  def emphasizedText(node: EmphasizedText): String = s"*${node.text}*"

  def standardText(node: StandardText): String = node.text

  def forcedNewLine(node: ForcedNewline): String = "\n\n"

  def rawText(node: RawText): String = node.text

  def unorderedList(node: UnorderedList): String = {
    ((for (line <- node.items) yield s"* ${line.map(translate(_)).mkString}") mkString "\n") + "\n\n"
  }

  def orderedList(node: OrderedList): String = {
    ((for (line <- node.items) yield s"${node.items.indexOf(line) + 1}. ${line.map(translate(_)).mkString}") mkString "\n") + "\n\n"
  }

  def weightedText(node: WeightedText): String = s"**${node.text}**"

  def markedText(node: MarkedText): String = s"${node.text}"

  def struckthroughText(node: StruckthroughText): String = s"~~${node.text}~~"

  def monospacedText(node: MonospaceText): String = {
    s"`${node.text}`"
  }

  def link(node: Link): String = s"[${translate(node)}](${node.to.target})"

  def quote(node: Quote): String = s"> ${translate(node)}\n"

  def code(node: Code): String = s"```${node.directives.getOrElse("lang", "")}\n${node.contents}\n```\n"

  def embed(node: EmbedNode): String = node match {
    case ImageEmbedNode(arguments, meta) => s"![${meta.getOrElse("alt", "")}](${arguments.head})"
    // only support image embeds for GHMD
    case _ => ""
  }
}

object GithubMDTranslator {
  def translate(node:Node): String = {
    (new GithubMDTranslator).translate(node)
  }
}
