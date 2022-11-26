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
  object Loop extends CompilerStatus

  case class CompilerState(validDefinition: List[Function1[State, State]] = List(), status: CompilerStatus = Compiling, message: String = "") {
    def validDefinition(function:Function1[State, State] ): CompilerState = this.copy(validDefinition = function :: this.validDefinition)
  }

  def compile(state: State): State = {
    val (wordName, wordDefinition) = state.input.splitAt(1)

    def parseWord(compilerState: CompilerState, word: String): CompilerState = {
      state.dictionary(word) match {
        case Some(Word("do", _, _, _)) => compilerState.copy(status = Loop)
        case Some(validWord) => compilerState.validDefinition(validWord.function)
        case None => NumberRunner(word) match {
          case someNumber => compilerState.validDefinition((someState: State) => someState.push(someNumber))
          case None => compilerState.copy(status = Aborting, message = s"Undefined $word")
        }
      }
    }

    val finalState = wordDefinition.foldLeft(CompilerState()) { (compilerState, word) =>
      compilerState.status match {
        case Compiling => parseWord(compilerState, word)
        case Aborting => compilerState
        case Loop => ???
      }
    }

    finalState match {
      case CompilerState(_, Aborting, message) => state.abort(message)
      case CompilerState(validDefinition, Compiling, _) =>
        val newDefinition = (state: State) => {
          validDefinition.foldRight(state) { (function, nextState: State) =>
            function(nextState)
          }
        }
        val newWord = Word(wordName.head, "TODO", newDefinition, s": ${state.input.mkString(" ")} ;")
        val newDictionary = Map[String, Word]((newWord.name, newWord)) ++ state.dictionary.dict
        state.copy(dictionary = Dictionary(newDictionary))
      case CompilerState(_, status, _) => state.abort(s"Compiler Error: invalid CompilerState $status")
    }
  }

  @tailrec
  def apply(state: State): State = {
    val currentInput = state.input
    currentInput match {
      case ":" +: compilerInput :+ ";" => compile(state.input(compilerInput))
      case ":" :: _ =>
        val read = readLine("\t\t")
        val nextInput = System.parseInputFromReadLine(read)
        val newInput = currentInput ++ nextInput
        Compiler(state.input(newInput))
      case _ => state.abort(s"Compiler Error: can't apply for input ${state.input} on State ${state.status}")
    }
  }
}
