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

package com.haaksmash.saxophone

import com.haaksmash.saxophone.emitters.{ConsoleEmitter, FileEmitter}
import com.haaksmash.saxophone.intakes.FileIntake
import com.haaksmash.saxophone.translators.{SaxophoneTreeStringTranslator, GithubMDTranslator, HTMLTranslator}


object Application {

  val help_string =
    """usage: saxophone [-d] [-l lanugage] [-o filename] input_filename
      |
      |Args:
      | -d              Print out debugging information
      | -l LANG         Specifies that the output should be in LANG;
      |                 can be one of [html, github, debug]; default is html.
      | -o OUT          Write the result to a file specified by OUT. If no
      |                 output file is specified, prints result to stdout.
    """.stripMargin

  /**
   * @param args [-d] [-l language] [-o filename] input_filename
   */
  def main(args: Array[String]): Unit = {
    if (args.contains("--help")) {
      println(help_string)
      return
    }

    val translator = {
      val language = {
        if (args.contains("-l")) {
          args(args.indexOf("-l") + 1)
        } else
          "html"
      }

      if (args.contains("-d"))
        println(s"language output: $language")

      language match {
        case "github" => new GithubMDTranslator
        case "debug" => new SaxophoneTreeStringTranslator
        case _ => new HTMLTranslator(true)
      }
    }

    val saxophone_filename = args(args.length - 1)

    val document_result = FileIntake(saxophone_filename)

    document_result match {
      case Some(document) =>
        if (args.contains ("-o")) {
          val output_filename = args (args.indexOf ("-o") + 1)
          FileEmitter(output_filename).emit(translator.translate(document))
        } else {
          ConsoleEmitter.emit(translator.translate(document))
        }

        if (args.contains ("-d"))
          ConsoleEmitter.emit(translator.translate(document))
      case _ =>
        System.err.println("Could not process input as a saxophone document")
        System.exit(1)
    }
  }
}
