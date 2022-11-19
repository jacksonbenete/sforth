package interpreter

import interpreter.DataStructures._

object DefaultDictionary {
  def take2(state: State[Any]): (Any, Any, State[Any]) = {
    val (x, xState) = state.pop()
    val (y, yState) = xState.pop()
    (x, y) match {
      case (None, None) => (None, None, state)
      case (_, None) => (None, None, state)
      case _ => (x.get, y.get, yState)
    }
  }

  def plus(name: String)(state: State[Any]): State[Any] = {
    take2(state) match {
      case (x: Int, y: Int, newState) => val res = x + y; newState.push(res)
      case (None, None, _) => println(s"Stack-underflow."); state.copy(specialFlag = Abort())
      case (x, y, _) =>
        println(s"$name not implemented for types: ${x.getClass} and ${y.getClass}")
        state
    }
  }

  def minus(name: String)(state: State[Any]): State[Any] = {
    take2(state) match {
      case (x: Int, y: Int, newState) => val res = y - x; newState.push(res)
      case (None, None, _) => println(s"Stack-underflow."); state.copy(specialFlag = Abort())
      case (x, y, _) =>
        println(s"$name not implemented for types: ${x.getClass} and ${y.getClass}")
        state
    }
  }

  def times(name: String)(state: State[Any]): State[Any] = {
    take2(state) match {
      case (x: Int, y: Int, newState) => val res = x * y; newState.push(res)
      case (None, None, _) => println(s"Stack-underflow."); state.copy(specialFlag = Abort())
      case (x, y, _) =>
        println(s"$name not implemented for types: ${x.getClass} and ${y.getClass}")
        state
    }
  }

  def divide(name: String)(state: State[Any]): State[Any] = {
    take2(state) match {
      case (x: Int, y: Int, newState) => val res = y / x; newState.push(res)
      case (None, None, _) => println(s"Stack-underflow."); state.copy(specialFlag = Abort())
      case (x, y, _) =>
        println(s"$name not implemented for types: ${x.getClass} and ${y.getClass}")
        state
    }
  }

  def pop(state: State[Any]): State[Any] = {
    val (item, newState) = state.pop()
    item match {
      case Some(value) => println(s"$value\t{${newState.stackSize}}")
      case None => println(s"{${state.stackSize}}")
    }
    newState
  }

  def showStack(name: String)(state: State[Any]): State[Any] = {
    state.head match {
      case None => println(s"{0}"); state
      case Some(value) => println(s"${value}\t{${state.stackSize}}"); state
    }
  }

//  def dup(state: State[Any]): State[Any] = {
//    state.head match {
//      case None =>
//        println(s"Stack-underflow.") ; state.copy(specialFlag = Abort())
//      case Some(Failure()) => state.copy(specialFlag = Abort())
//      case Some(value) => state.push(value)
//    }
//  }
  def dup(state: State[Any]): State[Any] = {
    state.pop match {
      case (None, _) =>
        println("Stack-underflow.")
        state.copy(specialFlag = Abort())
      case (Some(value), _) => state.push(value)
    }
  }

  def see(state: State[Any]): State[Any] = {
    val lastWord = state.input.split(" ").last
    val fnDefinition = state.dictionary(lastWord)
    fnDefinition match {
      case None =>
        println(s"Word $lastWord not defined.")
        state.copy(specialFlag = Abort())
      case Some(value) =>
        println(s"${value.function}")
        state.copy(specialFlag = Abort())
    }
  }

  def exit(state: State[Any]): State[Any] = {
    state.copy(specialFlag = Exit())
  }

  val dict: Dictionary[Any] = Dictionary[Any](Map[String, Word[Any]](
    ("exit", Word("exit", exit)),
    ("+", Word("+", plus(name = "+"))),
    ("-", Word("-", minus(name = "-"))),
    ("*", Word("*", times(name = "*"))),
    ("/", Word("/", divide(name = "/"))),
    (".", Word(".", pop)),
    (".s", Word(".s", showStack(name = ".s"))),
    ("dup", Word("dup", dup)),
    ("see", Word("see", see))
  ))
}
