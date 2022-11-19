package sforth.model

import sforth.model.DataStructures.Word
import sforth.model.Data._
import sforth.model.State._
import sforth.model.State.Status._

object DataStructures {
  case class Word(name: String, function: State => State)
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
        case (_, _, Abort) => state
        case (DataItem(Number, x: Int), DataItem(Number, y: Int), _) => newState.push(DataItem(Number, x + y))
        case (DataItem(Number, x), DataItem(Number, y), _) =>
          println(s"+ not implemented for types: ${x.getClass} and ${y.getClass}")
          state.abort
      }})

    val look = Word(".s", (state: State) => {
      state.look
      state
    })

    val exit = Word("exit", (state: State) => {
      state.copy(status = Exit)
    })

    Dictionary(Map[String, Word](
      (plus.name, plus),
      (look.name, look),
      (exit.name, exit)
    ))
  }
}
