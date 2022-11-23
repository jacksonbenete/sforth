package sforth.system

import sforth.model.State._
import sforth.model.State.Status._
import sforth.model.Dictionary
import sforth.model.Data._

import scala.io.StdIn.readLine

object REPL {
  val IMPLEMENTATION = "SForth"
  val VERSION = "0.1"

  def REPL(state: State): State = {
    print(s"${state.mark} ")

    val read = readLine().strip()
    val input = read.split(" ").toList

    // todo ":" should be a word
    def compile = ???

    val nextState = input match {
      case "" :: Nil => state
      case ":" +: _ :+ ";" => Compiler(state.copy(input = input))
      case ":" :: _ => state.abort("Missing compiler semicolon")
      case _ => Interpreter(state.copy(input = input))
    }

    nextState.status match {
      case Abort =>
        println(nextState.io)
        println("Aborting and recovering last valid state...")
        REPL(state)
      case Exit =>
        println(s"Dump state: ${nextState.toString}")
        nextState
      case Valid =>
        println(s"${nextState.io}\tok")
        REPL(nextState.copy(io = IO(List(""))))
      case Failure => ???
      case StackUnderflow =>
        println("Stack-underflow")
        REPL(state)
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Starting $IMPLEMENTATION v$VERSION...")

    val systemDictionary = Dictionary.systemDictionary
    val stack = List[Int]()
    val mark = ">"
    val namespace = Map[String, Dictionary]((mark, systemDictionary))
    val input = List[String]()
    val io = IO(List())
    val status = Valid

    val initialState = State(systemDictionary, stack, namespace, mark, input, io, status)

    REPL(initialState)
  }
}
