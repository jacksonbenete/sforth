package interpreter

import interpreter.DataStructures._
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object REPL {
  println("SForth Compiler Version: 0.1")

  def compiler[R](state: State[R]): State[R] = {
    ???
  }

  def interpreter[R](state: State[R]): State[R] = {
    state.input.split(" ").foldLeft(state) { (state: State[R], word: String) =>
      // see if word exists
      (state.dictionary(word), state.specialFlag) match {
        case (_, Abort()) => state
        case (Some(value), _) => {
          value.function(state)
        }
        case (None, _) =>
          // if word isn't on dict, try to parse it as a ValidType and put on Stack
          word.toIntOption match {
            case Some(value) => state.push(value)
            case None =>
              println(s"Word $word is undefined.")
              state.copy(specialFlag = Abort())
          }
      }
    }
  }

  println(s"SForth Interpreter Version: 0.1")
  @tailrec
  def REPL[R](state: State[R]): State[R] = {
    print(s"> ")

    val input = readLine().strip()

    val nextState = input.startsWith(": ") match {
      case true => compiler(state.copy(input = input))
      case false => interpreter(state.copy(input = input))
    }

    nextState.specialFlag match {
      case ValidState() => REPL(nextState)
      case Abort() => REPL(state) // abort and recover last state
      case Exit() => println(s"Dump state: ${nextState.toString}") ; nextState
    }
  }

  def main(args: Array[String]): Unit = {
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
