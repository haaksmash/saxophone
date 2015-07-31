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

import com.haaksmash.saxophone.primitives.{EOF, Line}

import scala.util.parsing.input.{Position, Reader}

class LineReader private(val lines: Seq[Line], val lookup: Option[Boolean], val linecount: Int)
  extends Reader[Line] {

  def this(ls: Seq[Line]) = this(ls, None, 1)

  override def first: Line = if (lines.isEmpty) EOF else lines.head

  override def atEnd: Boolean = lines.isEmpty

  override def pos: Position = new Position {
    def line = linecount

    override def column: Int = 1

    protected def lineContents = first.text
  }

  override def rest: Reader[Line] = if (atEnd) this else new LineReader(lines.tail, lookup, linecount + 1)
}

