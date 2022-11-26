package sforth.system

import sforth.model.DataStructures.Word
import sforth.model.State.State
import sforth.model.State.Status._

case object NumberRunner {
  def apply(state: State): State = {
    val currentWord = state.registers.input.toIntOption
    val currentStatus = state.status

    (currentWord, currentStatus) match {
      case (None, _) => state.abort(s"Undefined word `${state.registers.input}`")
      case (value, InterpretMode) =>
        state.push(value)
      case (value, CompileMode) =>
        val newNumber = Word("number", "", (state: State) => state.push(value))
        state.copy(registers = state.registers.copy(word = newNumber))
      case (_, status) => state.abort(s"NumberRunner Error: invalid status $status")
    }
  }
}
