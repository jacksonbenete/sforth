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

    val nextState = input.splitAt(1) match {
      case (List(":"), _) => Compiler(state.copy(input = input))
      case _ => Interpreter(state.copy(input = input))
    }

    nextState.status match {
      case Abort =>
        println("Aborting and recovering last valid state...")
        REPL(state)
      case Exit =>
        println(s"Dump state: ${nextState.toString}")
        nextState
      case Valid => REPL(nextState)
      case Failure => ???
      case StackUnderflow => ???
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Starting $IMPLEMENTATION v$VERSION...")

    val systemDictionary = Dictionary.systemDictionary
    val stack = List[DataItem]()
    val mark = ">"
    val namespace = Map[String, Dictionary]((mark, systemDictionary))
    val input = List[String]()
    val status = Valid

    val initialState = State(systemDictionary, stack, namespace, mark, input, status)

    REPL(initialState)
  }
}
