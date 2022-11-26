package sforth.system

import sforth.model.State.State
import sforth.model.State.Status._

object Executor {
  def apply(state: State): State = {
    val currentWord = state.registers.word
    val currentStatus = state.status

    currentStatus match {
      case InterpretMode => currentWord.function(state)
      case status => state.abort(s"Cannot EXECUTE word `${currentWord.name}`, invalid state `$status`")
    }
  }
}
