package tsforth

import tsforth.DataStructures._
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object REPL {
  @tailrec
  def REPL[R](state: State[R]): State[R] = {
    print(s"> ")

    val input = readLine().strip()

    val nextState = input.startsWith(": ") match {
      case true => Compiler(state.copy(input = input))
      case false => Interpreter(state.copy(input = input))
    }

    nextState.specialFlag match {
      case ValidState() => REPL(nextState)
      case Abort() => REPL(state) // abort and recover last state
      case Exit() => println(s"Dump state: ${nextState.toString}") ; nextState
    }
  }

  def main(args: Array[String]): Unit = {
    println("SForth Compiler Version: 0.1")
    println(s"SForth Interpreter Version: 0.1")
    println(s"Starting SForth...")

    val defaultDict = DefaultDictionary.dict

    val initialState = State[Any](
      dictionary = defaultDict,
      namespace = Map[String, Dictionary[Any]](("default", defaultDict)),
      stackPointer = List[StackType](),
      stackInt = List[Int](),
      stackWord = List[Word[Any]](),
      stackString = List[String](),
      stackSize = 0,
      input = "",
      specialFlag = ValidState()
    )

    REPL(initialState)
  }
}
