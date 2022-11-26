package sforth.system

import sforth.model.State._
import sforth.model.State.Status._
import sforth.model.Data._

object Interpreter {
  /**
   * Implemented following "Starting Forth" pg. 15.
   *
   * The text interpreter (a very elegant gentleman) scans
   * the input stream, looking for strings separated by spaces.
   *
   * Looks for the string in the Dictionary, and either gives
   * it for the Executor or the NumberRunner.
   *
   * @param state
   * @return State with updated status and IO for effect
   */
  def apply(state: State): State = {
    val finalState = state.input.foldLeft(state: State) { (state: State, word: String) =>
      (state.dictionary(word), state.status) match {
        case (Some(validWord), InterpretMode) => Executor(state.copy(registers = state.registers.word(validWord)))
//        case (None, InterpretMode) => NumberRunner(state.copy(registers = state.registers.input(word)))
        case (None, InterpretMode) => NumberRunner(word) match {
          case None => state.abort(s"Undefined word $word")
          case number => state.push(number)
        }
        case _ => state
      }
    }

    finalState.status match {
      // If it was still Interpreting until this point,
      // or returned successfully from Compiler, clean and return as Valid State
      case InterpretMode => finalState.out("\tok").cleanState.valid
      case CompileMode => finalState.out("\tok").cleanState.valid
      case _ => finalState
    }
  }
}
