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

package com.haaksmash.saxophone.readers

import scala.util.parsing.input.{Position, Reader}

class StringLineReader private(val lines: Seq[String], val lineCount: Int) extends Reader[String] {

  private val eofline = "EOF"

  def this(ls: Seq[String]) = this(ls, 1)

  def this(ls: String) = this(ls.split('\n'))

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
