package com.haaksmash.hst

import scala.util.parsing.input.{Position, Reader}

class LineReader private (val lines: Seq[Line], val lookup: Option[Boolean], val linecount: Int)
  extends Reader[Line] {

  def this(ls:Seq[Line]) = this(ls, None, 1)

  override def first: Line = if (lines.isEmpty) EOF else lines.head

  override def atEnd: Boolean = lines.isEmpty

  override def pos: Position = new Position {
    def line = linecount
    override def column: Int = 1
    protected def lineContents = first.text
  }

  override def rest: Reader[Line] = if (atEnd) this else new LineReader(lines.tail, lookup, linecount + 1)
}

