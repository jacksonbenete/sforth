package sforth.system

import sforth.model.State._
import sforth.model.State.Status._
import sforth.model.Dictionary
import sforth.model.Data._
import sforth.model.DataStructures.Word

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object System {
  val IMPLEMENTATION = "SForth"
  val VERSION = "0.2"

  def parseInputFromReadLine(string: String): List[String] =
    string.strip().toLowerCase.split(" ").toList.filterNot(_.isEmpty)
  @tailrec
  def REPL(state: State): State = {
    val read = readLine(s"${state.mark} ")
    val input = parseInputFromReadLine(read)

    val eval = Dictionary.systemDictionary("interpret")
      .map((word: Word) => word.function(state.input(input))) match {
            case Some(newState) => newState
            case None => state.abort(s"REPL Error: INTERPRET couldn't eval `$read`")
          }
//    val eval = Interpreter(state.interpretMode.input(input))

    println {
      eval.status match {
        case Exit => s"DUMP: ${eval.toString}"
        case Valid => eval.io
        case StackUnderflow => "Stack-underflow"
        case Abort => s"${eval.io}\nAborting and recovering last valid state..."
        case status => s"REPL Error: REPL shouldn't be on state $status, recovering last valid state..."
      }
    }
    // LOOP
    eval.status match {
      case Exit => state
      case Valid => REPL(eval.copy(io = IO(List(""))))
      case _ => REPL(state)
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Starting $IMPLEMENTATION v$VERSION...")

    val systemDictionary = Dictionary.systemDictionary
    val stack = List[Int]()
    val registers = Registers()
    val mark = ">"
    val namespace = Map[String, Dictionary]((mark, systemDictionary))
    val input = List[String]()
    val io = IO(List())
    val status = Valid

    val initialState = State(systemDictionary, stack, registers, namespace, mark, input, io, status)

    REPL(initialState)
  }
}
