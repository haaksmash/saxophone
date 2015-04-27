package com.haaksmash.saxophone.readers

import scala.util.parsing.input.{Position, Reader}

class StringLineReader private (val lines: Seq[String], val lineCount: Int) extends Reader[String] {

  private val eofline = "EOF"

  def this(ls:Seq[String]) = this(ls, 1)
  def this(ls:String) = this(ls.split('\n'))

  override def first: String = if (lines.isEmpty) eofline else lines.head

  override def atEnd: Boolean = lines.isEmpty

  override def pos: Position = new Position {
    def line = lineCount
    override def column: Int = 1
    override protected def lineContents: String = first
  }

  override def rest: Reader[String] = if (lines.isEmpty) this else new StringLineReader(lines.tail, lineCount + 1)

  override def toString = "StringLineReader:<" + first + ">"
}
