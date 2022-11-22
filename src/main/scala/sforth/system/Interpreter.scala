package sforth.system

import sforth.model.State._
import sforth.model.State.Status._
import sforth.model.Data._

object Interpreter {

  def parseWord(word: String, state: State): State = {
    word.toIntOption match {
      case None => state.abort(s"Word $word doesn't exists on dictionary or ${state.mark} namespace")
      case Some(value) => state.push(DataItem(Number, value))
    }
  }

  def apply(state: State): State = {
    // for each word on input string
    val finalState = state.input.foldLeft(state: State) { (state: State, word: String) =>
      // see if word exists in dictionary
      (state.dictionary(word), state.status) match {
        // if state is corrupt, don't execute or parse any other word, but raise corrupt status
        case (_, Abort) => state
        case (_, Exit) => state
        case (_, StackUnderflow) => state
        // if word doesn't exists on dictionary, parse it to put on Stack
        case (None, _) => parseWord(word, state)
        // if word exists on dictionary, evaluate function
        case (Some(validWord), _) => validWord.function(state)
      }
    }
    // clean state (e.g. reverse output list)
    finalState.copy(io = IO(finalState.io.data.filter(!_.isEmpty).reverse))
  }
}
