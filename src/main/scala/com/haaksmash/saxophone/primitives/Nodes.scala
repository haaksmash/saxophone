package com.haaksmash.saxophone

sealed abstract class Node {
  def children: Traversable[Node]
  val label = "node"
  def label_display = s"$label"
  override def toString = s"<$label_display>${children.foldLeft("")((prev, current) => prev + current.toString)}</$label>"
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
  override def toString = s"""<$label ${(directives map {case (k, v) => s"""$k="$v""""}).foldLeft("")((prev, current) => s"$prev $current")}>$contents</$label>"""
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


trait InlineNode extends Node


case class Footnote(children: Seq[Node]) extends InlineNode {
  override val label = "foot"
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


case class Link(override val children: Seq[Node], to: LinkTarget) extends InlineNode {
  override val label = "a"
  override def label_display = s"$label target=$to"
}

case class LinkTarget(target: String) extends InlineNode {
  val children = Seq.empty
  override def toString = target
}


sealed abstract class ListNode(items: Traversable[Node]) extends Node {
  def children = items
}

case class OrderedList(items: Seq[Node]) extends ListNode(items) {
  override val label = "ol"
}

case class UnorderedList(items: Set[Node]) extends ListNode(items){
  override val label = "ul"
}
