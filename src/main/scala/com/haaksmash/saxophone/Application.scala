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
import com.haaksmash.saxophone.translators.{GithubMDTranslator, HTMLTranslator, SaxophoneTreeStringTranslator}

import scala.util.{Failure, Success}


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

    val emitter = if (args.contains("-o")) {
      val output_filename = args(args.indexOf("-o") + 1)
      FileEmitter(output_filename)
    } else {
      ConsoleEmitter
    }

    val saxophone_filename = args(args.length - 1)

    val pipe = Pipeline
      .on(saxophone_filename)
      .from(new FileIntake)
      .via(translator)
      .to(emitter)

    pipe.process() match {
      case Success(s) =>
        if (args.contains("-d"))
          ConsoleEmitter.emit(s)
      case Failure(ex) =>
        System.err.println(ex.getMessage)
        System.exit(1)
    }

  }
}
