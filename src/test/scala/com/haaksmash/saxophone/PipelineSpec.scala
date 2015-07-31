package com.haaksmash.saxophone

import com.haaksmash.saxophone.emitters.BaseEmitter
import com.haaksmash.saxophone.intakes.BaseIntake
import com.haaksmash.saxophone.primitives.Node
import com.haaksmash.saxophone.translators.BaseTranslator
import org.scalatest.FlatSpec

import scala.util.Try

class PipelineSpec extends FlatSpec {

  "Pipeline" should "process to Failure when no input" in {
    val pipe = new Pipeline(None, None, None, None).process()
    assert(pipe.isFailure)
  }

  it should "have defaults aside from input" in {
    val pipe = new Pipeline(None, None, None, Some("lalala")).process()
    assert(pipe.isSuccess)
  }

  "Pipeline.from" should "not modify itself but return new Pipelines" in {
    val pipe = new Pipeline(None, None, None, None)
    val new_pipe = pipe.from(
      new BaseIntake {
        override def intake(input: String): Try[String] = ???
      }
    )

    assert(pipe != new_pipe)
  }

  "Pipeline.via" should "not modify itself but return new Pipelines" in {
    val pipe = new Pipeline(None, None, None, None)
    val new_pipe = pipe.via(
      new BaseTranslator {
        override def translate(node: Node): String = ???
      }
    )

    assert(pipe != new_pipe)
  }

  "Pipeline.to" should "not modify itself but return new Pipelines" in {
    val pipe = new Pipeline(None, None, None, None)
    val new_pipe = pipe.to(
      new BaseEmitter {
        override def emit(contents: String): Unit = ???
      }
    )

    assert(pipe != new_pipe)
  }

  "Pipeline.on" should "not modify itself but return new Pipelines" in {
    val pipe = new Pipeline(None, None, None, None)
    val new_pipe = pipe.on("lalala")

    assert(pipe != new_pipe)
  }

  "Pipeline.process" should "run an input through all the stages of the pipe" in {
    var operations = Seq.empty[String]
    val pipe = new Pipeline(
      Some(
        new BaseIntake {
          override def intake(input: String): Try[String] = Try {
            operations = operations :+ "intake"
            input
          }
        }
      ),
      Some(
        new BaseTranslator {
          override def translate(node: Node): String = {
            operations = operations :+ "translate"
            "translation"
          }
        }
      ),
      Some(
        new BaseEmitter {
          override def emit(contents: String): Unit = {
            operations = operations :+ "emit"
          }
        }
      ),
      Some("input")
    )

    val output = pipe.process()

    assert(output.isSuccess)
    assert(operations == Seq("intake", "translate", "emit"))
  }

}
