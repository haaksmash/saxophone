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
  def rawText(node:RawText): String

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
    case n: RawText => rawText(n)
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

  private def translateSingle(node:Node): String = {
    node_to_translator(node)
  }
}
