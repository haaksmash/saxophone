package com.haaksmash.hst

sealed abstract trait Line {
  def text: String
  def payload = text
}
case class HeadingLine(prefix:String, text:String) extends Line {
  override val payload: String = {
    text.trim.reverse.dropWhile(_ == '#').reverse
  }

  val headerLevel = prefix.length

  override def toString = "HeadingLine(" + headerLevel + ", " + payload + ")"
}
case class TextLine(text:String) extends Line

case class EmptyLine(text:String="") extends Line

case class CodeStartLine(directives:Map[String, String], text:String="") extends Line

case class CodeEndLine(text:String="") extends Line

case class QuoteLine(text:String) extends Line {
  override val payload: String = text.trim.reverse.dropWhile(_ == '>').reverse.trim
  override def toString = "QuoteLine(" + payload + ")"
}


sealed abstract trait ListLine extends Line {
  override val payload: String = text.trim
  override def toString = getClass.getName + "(" + text + ")"
}

case class OrderedLine(text:String) extends ListLine

case class UnorderedLine(text:String) extends ListLine

object EOF extends Line {
  val text = "EOF"
}
