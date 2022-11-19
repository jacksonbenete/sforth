package tsforth

import tsforth.DataStructures._

object Interpreter {
  def apply[R](state: State[R]): State[R] = {
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
}
