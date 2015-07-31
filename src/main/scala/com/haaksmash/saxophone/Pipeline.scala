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

import com.haaksmash.saxophone.emitters.BaseEmitter
import com.haaksmash.saxophone.intakes.{BaseIntake, StringIntake}
import com.haaksmash.saxophone.parsers.{BlockParsers, LineParsers}
import com.haaksmash.saxophone.readers.{LineReader, StringLineReader}
import com.haaksmash.saxophone.translators.{BaseTranslator, HTMLTranslator}

import scala.util.{Failure, Success, Try}


trait PipelineDefinition {
  def intake: Option[BaseIntake] = None

  def translator: Option[BaseTranslator] = None

  def emitter: Option[BaseEmitter] = None

  def input: Option[String] = None

  def from[T <: BaseIntake](intake: T): PipelineDefinition

  def via[T <: BaseTranslator](translator: T): PipelineDefinition

  def to[T <: BaseEmitter](emitter: T): PipelineDefinition

  def on(input: String): PipelineDefinition
}


class Pipeline(
  intake: Option[BaseIntake],
  translator: Option[BaseTranslator],
  emitter: Option[BaseEmitter],
  input: Option[String]
) extends PipelineDefinition {

  override def from[T <: BaseIntake](intake: T): Pipeline = new Pipeline(
    Some(intake),
    translator,
    emitter,
    input
  )

  override def to[T <: BaseEmitter](emitter: T): Pipeline = new Pipeline(
    intake,
    translator,
    Some(emitter),
    input
  )

  override def via[T <: BaseTranslator](translator: T): Pipeline = new Pipeline(
    intake,
    Some(translator),
    emitter,
    input
  )

  override def on(input: String) = new Pipeline(
    intake,
    translator,
    emitter,
    Some(input)
  )

  def process(): Try[String] = {
    if (input.isEmpty)
      return Failure(new RuntimeException("can't process without input"))

    val intake_result = intake match {
      case Some(i) => i.intake(input.get)
      case None => StringIntake(input.get)
    }

    if (intake_result.isFailure)
      return intake_result

    val translate_result = intake_result
      .map(
        in =>
          (new LineParsers).lines(new StringLineReader(in))
      )
      .map(
        lines => {
          println(lines)
          (new BlockParsers).blocks(new LineReader(lines.get))}
      )
      .map(
        doc => {
          println(doc)
          translator match {
            case Some(t) => t.translate(doc.get)
            case None => HTMLTranslator.translate(doc.get)
          }}
      )

    if (translate_result.isFailure)
      return translate_result

    val emit_result = translate_result.map(
      in =>
        emitter match {
          case Some(e) =>
            e.emit(in)
            in
          case None => in
        }
    )

    return emit_result match {
      case Success(r) => Success(r)
      case Failure(_) => Failure(new RuntimeException("could not emit result"))
    }
  }
}

object Pipeline extends PipelineDefinition {
  override def from[T <: BaseIntake](intake: T): Pipeline = new Pipeline(
    Some(intake),
    None,
    None,
    None
  )

  override def to[T <: BaseEmitter](emitter: T): Pipeline = new Pipeline(
    None,
    None,
    Some(emitter),
    None
  )

  override def via[T <: BaseTranslator](translator: T): Pipeline = new Pipeline(
    None,
    Some(translator),
    None,
    None
  )

  override def on(input: String) = new Pipeline(
    None,
    None,
    None,
    Some(input)
  )
}

