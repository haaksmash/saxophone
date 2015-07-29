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

sealed abstract trait Line {
  def text: String

  def payload = text
}

case class HeadingLine(prefix: String, text: String) extends Line {
  override val payload: String = {
    text.trim.reverse.dropWhile(_ == '#').reverse
  }

  val headerLevel = prefix.length

  override def toString = "HeadingLine(" + headerLevel + ", " + payload + ")"
}

case class TextLine(text: String) extends Line

case class EmbedLine(arguments: Seq[String], text:String, meta:Map[String,String]) extends Line

case class EmptyLine(text: String = "") extends Line

case class CodeStartLine(directives: Map[String, String], text: String = "") extends Line

case class CodeEndLine(text: String = "") extends Line

case class QuoteLine(text: String) extends Line {
  override val payload: String = text.trim.dropWhile(_ == '>').trim

  override def toString = "QuoteLine(" + payload + ")"
}


sealed abstract trait ListLine extends Line {
  def glyph: String

  def raw_text: String

  override val payload: String = raw_text.trim

  override def text = glyph + " " + payload

  override def toString = getClass.getName + "(" + text + ")"
}

case class OrderedLine(glyph: String, raw_text: String) extends ListLine

case class UnorderedLine(glyph: String, raw_text: String) extends ListLine

object EOF extends Line {
  val text = "EOF"
}
