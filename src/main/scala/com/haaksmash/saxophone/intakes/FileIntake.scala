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

import java.io.File

import com.haaksmash.saxophone.primitives.Document

class FileIntake extends BaseIntake {
  type IntakeType = File

  def intake(filename:IntakeType): Option[Document] = {

    val article_source = scala.io.Source.fromFile(filename)
    val the_article = article_source.mkString
    article_source.close()

    StringIntake(the_article)
  }
}

object FileIntake {
  def apply(filename: String) = {
    (new FileIntake).intake(new File(filename))
  }

  def apply(file:File) = {
    (new FileIntake).intake(file)
  }
}