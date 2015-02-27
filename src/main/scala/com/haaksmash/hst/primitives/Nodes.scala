package com.haaksmash.hst

sealed abstract trait Node {
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

case class Footnote(children: Seq[Node]) extends Node {
  override val label = "foot"
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

case class Quote(children: Seq[Node], source: Option[Node]) extends Node {
  override val label = "quote"
  override def label_display = {
    source match {
      case Some(s) =>
        s"$label source=$s"
      case _ => s"$label"
    }
  }
}


abstract trait TransformedText extends Node {
  val children = Seq.empty
}

case class StandardText(text:String) extends TransformedText {
  override def toString = "<text>" + text + "</text>"
}

case class EmphasizedText(text:String) extends TransformedText

case class WeightedText(weight:Int, text:String) extends TransformedText

case class UnderlinedText(text:String) extends TransformedText

case class MonospaceText(text:String) extends TransformedText


case class Link(children: Seq[Node], to: LinkTarget) extends Node {
  override val label = "a"
  override def label_display = s"$label target=$to"
}

case class LinkTarget(target: String) extends Node {
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
