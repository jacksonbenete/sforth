package interpreter

import interpreter.DataStructures._
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object REPL {
  println("SForth Compiler Version: 0.1")

  def compiler[R](state: State[R]): State[R] = {
    // TODO: currently not accepting redefining word that already exist
    // TODO: breaks compiler in future version if input doesn't ends with semicolon
    if (!state.input.endsWith(";")) println(s"WARNING: compiler needs `;` (semicolon) terminate definition symbol.")

    // create a new dictionary entry with newWorld that returns a function that executes every word in the array
    val (newWorldList, definitionList) = state.input.replaceFirst(": ", "").replaceFirst(";$", "").split(" ").toList.splitAt(1)
    val newWorldName = newWorldList.head

    // first validate if all words on definition exists
    val validateWords = definitionList.foldLeft((true, "", List[Word[R]]())) {
      (stillValid: Tuple3[Boolean, String, List[Word[R]]], word: String) =>
        val (stillValidFlag, invalidWord, validWordList) = stillValid
        (stillValidFlag, state.dictionary(word)) match {
          case (false, _) => (false, invalidWord, validWordList)
          case (true, None) => (false, word, validWordList)
          case (true, Some(validWord)) => (true, "", validWord :: validWordList)
        }
    }
    validateWords match {
      case (false, invalidWord, _) =>
        println(s"Word $invalidWord is undefined.")
        state.copy(specialFlag = Abort())
      case (true, _, validWordList) =>
        val fnDefinition = (state: State[R]) => {
          // foldLeft would evaluate in the wrong order
          validWordList.foldRight(state) { (word: Word[R], nextState: State[R]) =>
            word.function(nextState)
          }
        }
        val newWord = Word(newWorldName, fnDefinition)
        val newDictionary = Map[String, Word[R]]((newWorldName, newWord)) ++ state.dictionary.dict
        state.copy(dictionary = Dictionary(newDictionary))
    }
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
