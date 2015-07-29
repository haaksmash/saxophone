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

import org.scalatest._

import java.io.File

class FileIntakeSpec extends FlatSpec {

  val article_filepath = getClass.getResource("/article_example.sax").getPath

  "FileIntake" should "successfully evaluate a .sax file by path" in {
    val output = FileIntake(article_filepath)

    assert(output.isSuccess)
  }

  it should "evaluate a .sax file directly" in {
    val article = new File(article_filepath)
    val output = FileIntake(article)

    assert(output.isSuccess)
  }
}
