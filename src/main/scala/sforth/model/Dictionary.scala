package sforth.model

import sforth.model.DataStructures.Word
import sforth.model.Data._
import sforth.model.State._
import sforth.model.State.Status._

object DataStructures {
  case class Word(name: String, glossary: String = "", function: State => State) {
    def load: Tuple2[String, Word] = (this.name, this)
  }
}

case class Dictionary(dict: Map[String, Word]) {
  def apply(key: String): Option[Word] = {
    this.dict.get(key)
  }
}

object Dictionary {
  val systemDictionary: Dictionary = {
    val plusfn = { (state: State) =>
      val (x, y, newState) = state.take2()
      newState.push(x.flatMap(x => y.map(y => x + y)))
    }
    val plus = Word("+", "(n1 n2 -- sum)", plusfn)

    val timesfn = { (state: State) =>
      val (x, y, newState) = state.take2()
      newState.push(x.flatMap(x => y.map(y => x * y)))
    }
    val times = Word("*", "(n1 n2 -- mul)", timesfn)

    val minusfn = { (state: State) =>
      val (x, y, newState) = state.take2()
      newState.push(x.flatMap(x => y.map(y => y - x)))
    }
    val minus = Word("-", "(n1 n2 -- sub)", minusfn)

    val divfun = { (state: State) =>
      val (x, y, newState) = state.take2()
      newState.push(x.flatMap(x => y.map(y => y / x)))
    }
    val divide = Word("/", "(n1 n2 -- div", divfun)

    val pop = Word(".", "(n -- )", (state: State) => {
      state.pop() match {
        case (Some(data), newState@State(_, _, _, _, _, _, Valid)) => newState.out(s"${data}\t{${newState.stackSize}}")
        case (_, newState@_) => newState
      }
    })

    val look = Word(".s", "( -- )", (state: State) => {
      state.look
    })

    val dup = Word("dup", "(n -- n1 n2)", (state: State) => {
      state.stack.headOption match {
        case None =>
          state.stackUnderflow
        case data =>
          state.push(data)
      }
    })

    val see = Word("see", "( -- )", (state: State) => {
      val word = state.input.last
      state.dictionary(word) match {
        case Some(word) => state.abort(s"${word.function}")
        case None => state.abort(s"Word $word undefined")
      }
    })

    val exit = Word("exit", "( -- )", (state: State) => {
      state.copy(status = Exit)
    })

    Dictionary(Map[String, Word](
      plus.load,
      minus.load,
      times.load,
      divide.load,
      pop.load,
      look.load,
      dup.load,
      see.load,
      exit.load
    ))
  }
}
