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

package com.haaksmash.saxophone.primitives

sealed abstract class Node {
  def children: Traversable[Node]
  val label = "node"
  def label_display = s"$label"
  override def toString = s"<$label_display>${children.mkString("")}</$label>"
}

case class Document(children: Seq[Node]) extends Node {
  override val label = "doc"
}

case class Heading(level: Int, children: Seq[Node]) extends Node {
  override val label = s"H"
  override def label_display = s"$label level=$level"
}

case class Paragraph(children: Seq[Node]) extends Node {
  override val label = "p"
}

case class ForcedNewline() extends Node {
  val children = Seq.empty
  override val toString = "<br/>"
}

case class Code(directives: Map[String, String], contents:String) extends Node {
  val children = Seq.empty
  override val label = "code"
  override def toString = s"""<$label ${(directives map {case (k, v) => s"""$k="$v""""}).mkString(" ")}>$contents</$label>"""
}

case class Quote(children: Seq[Node], source: Option[Seq[InlineNode]]) extends Node {
  override val label = "quote"
  override def label_display = {
    source match {
      case Some(s) =>
        s"$label source=$s"
      case _ => s"$label"
    }
  }
}


sealed abstract class ListNode(items: Traversable[Seq[Node]]) extends Node {
  /**
   * `children` is not a very useful method to use on ListNodes; because
   * they, by nature, have their "natural" children (the bulleted/numbered blocks)
   * and the blocks themselves have children, operating on all of the children
   * loses some of that information.
   * @return all the children of this List node
   */
  def children = items.flatten
}

case class OrderedList(items: Seq[Seq[Node]], present_unordered: Boolean = false) extends ListNode(items) {
  override val label = "ol"
}

case class UnorderedList(items: Set[Seq[Node]]) extends ListNode(items){
  override val label = "ul"
}


trait InlineNode extends Node


case class Footnote(children: Seq[InlineNode]) extends InlineNode {
  override val label = "foot"
}

case class RawText(text:String) extends InlineNode {
  val children = Seq.empty[Node]
  override def toString = "<raw>" + text + "</raw>"
}

trait TransformedText extends InlineNode {
  val children = Seq.empty[Node]
  def text:String
  override def toString = s"<$label_display>$text</$label>"
}

case class StandardText(text:String) extends TransformedText {
  override def toString = "<text>" + text + "</text>"
}

case class EmphasizedText(text:String) extends TransformedText {
  override val label = "em"
}

case class WeightedText(weight:Int, text:String) extends TransformedText {
  override val label = "strong"
}

case class UnderlinedText(text:String) extends TransformedText {
  override val label = "u"
}

case class MonospaceText(text:String) extends TransformedText {
  override val label = "mono"
}

case class StruckthroughText(text:String) extends TransformedText {
  override val label = "strike"
}


case class Link(override val children: Seq[InlineNode], to: LinkTarget) extends InlineNode {
  override val label = "a"
  override def label_display = s"$label target=$to"
}

case class LinkTarget(target: String) extends InlineNode {
  val children = Seq.empty
  override def toString = target
}
