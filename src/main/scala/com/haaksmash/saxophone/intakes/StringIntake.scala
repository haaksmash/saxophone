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

package com.haaksmash.saxophone.intakes

import com.haaksmash.saxophone.parsers.{LineParsers, BlockParsers}
import com.haaksmash.saxophone.primitives.Document
import com.haaksmash.saxophone.readers.{LineReader, StringLineReader}

import scala.util.Try

class StringIntake extends BaseIntake {
  override def intake(input: String): Try[String] = Try(input)
}

object StringIntake {
  def apply(input:String): Try[String] = {
    new StringIntake().intake(input)
  }
}
