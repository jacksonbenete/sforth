package sforth.system

import sforth.model.DataStructures.Word
import sforth.model.Dictionary
import sforth.model.State.Status._
import sforth.model.State._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Compiler {
  sealed trait CompilerStatus
  object Compiling extends CompilerStatus
  object Aborting extends CompilerStatus

  case class CompilerState(validDefinition: List[Word] = List(), status: CompilerStatus = Compiling, message: String = "") {
    def validDefinition(word: Word ): CompilerState = this.copy(validDefinition = word :: this.validDefinition)
    def compiling: CompilerState = this.copy(status = Compiling)
  }

  def doLoop(compilerState: CompilerState): CompilerState = {
    // create a new word that works on a loop condition
    val reversedInput = compilerState.validDefinition.reverse
    val loopBeginning = reversedInput.indexWhere(_.name == "do")
    val (_, insideLoop) = reversedInput.splitAt(loopBeginning+1)

    val newDefinition = (state: State) => {
        val (first, second, nextState) = state.take2()
        (first, second) match {
          case (None, _) => nextState.stackUnderflow
          case (_, None) => nextState.stackUnderflow
          case (Some(counter), Some(delimiter)) =>
            val loopWord = Range.apply(counter, delimiter - 1).toList.map { _ =>
              (state: State) => {
                insideLoop.foldLeft(state) { (someState, word) =>
                  word.function(someState)
                }
              }
            }
            loopWord.foldLeft(state) { (someState, function) =>
              function(someState)
            }
      }
    }
    compilerState.validDefinition(Word("do-loop", "", newDefinition, compilerState.validDefinition.mkString(" ")))
  }

  def compile(state: State): State = {
    val (wordName, wordDefinition) = state.input.splitAt(1)

    def parseWord(compilerState: CompilerState, word: String): CompilerState = {
      state.dictionary(word) match {
        case Some(Word("loop", _, _, _)) => doLoop(compilerState)
        case Some(validWord) => compilerState.validDefinition(validWord)
        case None => NumberRunner(word) match {
          case None => compilerState.copy(status = Aborting, message = s"Undefined word $word")
          case someNumber => compilerState.validDefinition(Word(word, "", (someState: State) => someState.push(someNumber), ""))
        }
      }
    }

    val finalState = wordDefinition.foldLeft(CompilerState()) { (compilerState, word) =>
      compilerState.status match {
        case Compiling => parseWord(compilerState, word)
        case Aborting => compilerState
      }
    }

    finalState match {
      case CompilerState(_, Aborting, message) =>
        state.abort(message)
      case CompilerState(validDefinition, Compiling, _) =>
        val newDefinition = (state: State) => {
          validDefinition.foldRight(state) { (word, nextState: State) =>
            word.function(nextState)
          }
        }
        val newWord = Word(wordName.head, "TODO", newDefinition, s": ${state.input.mkString(" ")} ;")
        val newDictionary = Map[String, Word]((newWord.name, newWord)) ++ state.dictionary.dict
        state.copy(dictionary = Dictionary(newDictionary))
      case CompilerState(_, status, _) =>
        state.abort(s"Compiler Error: invalid CompilerState $status")
    }
  }

  // TODO Should return State and meaningful error message
  def isCompilerInputValid(compilerInput: List[String]): Boolean = {
    val hasDo = compilerInput.contains("do")
    val hasLoop = compilerInput.contains("loop")
    (hasDo, hasLoop) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }
  }

  @tailrec
  def apply(state: State): State = {
    val currentInput = state.input
    currentInput match {
      case ":" +: compilerInput :+ ";" =>
        if (isCompilerInputValid(compilerInput)) compile(state.input(compilerInput)) else state.abort("Invalid input for CompileMode...")
      case ":" :: _ =>
        val read = readLine("\t\t")
        val nextInput = System.parseInputFromReadLine(read)
        val newInput = currentInput ++ nextInput
        Compiler(state.input(newInput))
      case _ => state.abort(s"Compiler Error: can't apply for input ${state.input} on State ${state.status}")
    }
  }
}
