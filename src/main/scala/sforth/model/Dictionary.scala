package sforth.model

import sforth.model.DataStructures.Word
import sforth.model.Data._
import sforth.model.State._
import sforth.model.State.Status._

object DataStructures {
  case class Word(name: String, function: State => State) {
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
    val plus = Word("+", (state: State) => {
      val (data1, data2, newState) = state.take2
      (data1, data2, newState.status) match {
        case (_, _, Abort) => state.abort
        case (_, _, StackUnderflow) => state.stackUnderflow
        case (DataItem(Number, x: Int), DataItem(Number, y: Int), _) => newState.push(DataItem(Number, x + y))
        case (DataItem(Number, x), DataItem(Number, y), _) => state.abort(s"+ not implemented for types: ${x.getClass} and ${y.getClass}")
      }})

    val times = Word("*", (state: State) => {
      val (data1, data2, newState) = state.take2
      (data1, data2, newState.status) match {
        case (_, _, Abort) => state.abort
        case (_, _, StackUnderflow) => state.stackUnderflow
        case (DataItem(Number, x: Int), DataItem(Number, y: Int), _) => newState.push(DataItem(Number, x * y))
        case (DataItem(Number, x), DataItem(Number, y), _) => state.abort(s"* not implemented for types: ${x.getClass} and ${y.getClass}")
      }
    })

    val minus = Word("-", (state: State) => {
      val (data1, data2, newState) = state.take2
      (data1, data2, newState.status) match {
        case (_, _, Abort) => state.abort
        case (_, _, StackUnderflow) => state.stackUnderflow
        case (DataItem(Number, x: Int), DataItem(Number, y: Int), _) => newState.push(DataItem(Number, y - x))
        case (DataItem(Number, x), DataItem(Number, y), _) => state.abort(s"- not implemented for types: ${x.getClass} and ${y.getClass}")
      }
    })

    val divide = Word("/", (state: State) => {
      val (data1, data2, newState) = state.take2
      (data1, data2, newState.status) match {
        case (_, _, Abort) => state.abort
        case (_, _, StackUnderflow) => state.stackUnderflow
        case (DataItem(Number, x: Int), DataItem(Number, y: Int), _) => newState.push(DataItem(Number, y / x))
        case (DataItem(Number, x), DataItem(Number, y), _) => state.abort(s"/ not implemented for types: ${x.getClass} and ${y.getClass}")
      }
    })

    val pop = Word(".", (state: State) => {
      state.pop() match {
        case (dataItem: DataItem, newState@State(_, _, _, _, _, _, Valid)) => newState.out(s"${dataItem.item}\t{${newState.stackSize}}")
        case (_: DataItem, newState@State(_, _, _, _, _, _, _)) => newState
      }
    })

    val look = Word(".s", (state: State) => {
      state.look
    })

    val dup = Word("dup", (state: State) => {
      state.stack.headOption match {
        case None =>
          state.stackUnderflow
        case Some(dataItem: DataItem) =>
          state.push(dataItem)
      }
    })

    val see = Word("see", (state: State) => {
      val word = state.input.last
      state.dictionary(word) match {
        case Some(word) => state.abort(s"${word.function}")
        case None => state.abort(s"Word $word undefined")
      }
    })

    val exit = Word("exit", (state: State) => {
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
