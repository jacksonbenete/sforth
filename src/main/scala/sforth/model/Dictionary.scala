package sforth.model

import sforth.model.DataStructures.Word
import sforth.model.Data._
import sforth.model.State._
import sforth.model.State.Status._
import sforth.system.{Compiler, Executor, Interpreter, NumberRunner}

object DataStructures {
  case class Word(name: String, glossary: String = "", function: State => State, definition: String = "") {
    /**
     * Returns a tuple in a valid format for the Dictionary.
     *
     * @return a new entry of type Map[String, Word]
     */
    def load: Tuple2[String, Word] = (this.name, this)

    override def toString: String =
      s"""
         |Name: $name
         |Glosary: $glossary
         |Definition: $definition
         |Function: $function
         |""".stripMargin
  }
}

case class Dictionary(dict: Map[String, Word]) {
  def apply(key: String): Option[Word] = {
    this.dict.get(key)
  }
}

object Dictionary {
  val systemDictionary: Dictionary = {
    val interpret = Word("interpret", "Enters Interpret Mode",
      (state: State) => Interpreter(state.interpretMode))
    val compile = Word(":", "Enters Compile Mode",
      (state: State) => Compiler(state.compileMode))
    val semicolon = Word(";", "Enters Compile Mode",
      (state: State) => state.status match {
        case CompileMode => state.valid
        case status => state.abort(s"Dictionary Error: word ; is not valid on State $status")
      })
    val number = Word("number", "Number-runner",
      (state: State) => state) // TODO
    val execute = Word("execute", "Execute the definition of a word",
      (state: State) => Executor(state))

    val plusfn = { (state: State) =>
      val (x, y, newState) = state.take2()
      x.flatMap(x => y.map(y => x + y)) match {
        case None => newState.stackUnderflow
        case value => newState.push(value)
      }
    }
    val plus = Word("+", "(n1 n2 -- sum)", plusfn)

    val timesfn = { (state: State) =>
      val (x, y, newState) = state.take2()
      x.flatMap(x => y.map(y => x * y)) match {
        case None => newState.stackUnderflow
        case value => newState.push(value)
      }
    }
    val times = Word("*", "(n1 n2 -- mul)", timesfn)

    val minusfn = { (state: State) =>
      val (x, y, newState) = state.take2()
      x.flatMap(x => y.map(y => y - x)) match {
        case None => newState.stackUnderflow
        case value => newState.push(value)
      }
    }
    val minus = Word("-", "(n1 n2 -- sub)", minusfn)

    val divfun = { (state: State) =>
      val (x, y, newState) = state.take2()
      x.flatMap(x => y.map(y => y / x)) match {
        case None => newState.stackUnderflow
        case value => newState.push(value)
      }
    }
    val divide = Word("/", "(n1 n2 -- div", divfun)

    val dot = Word(".", "(n -- )", (state: State) => {
      val (data, nextState) = state.pop()
      (data, nextState.status) match {
        case (Some(data), InterpretMode) => nextState.out(s"${data}\t{${nextState.stackSize}}")
        case (Some(data), CompileMode) => nextState.out(s"${data}\t{${nextState.stackSize}}")
        case (_, _) => nextState
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
        case Some(word) => state.abort(s"${word}")
        case None => state.abort(s"Word $word undefined")
      }
    })

    val exit = Word("exit", "( -- )", (state: State) => {
      state.copy(status = Exit)
    })

    // Chapter 1
    val spacesfn = { (state: State) =>
      val (x, newState) = state.pop()
      x match {
        case Some(value) => List.range(0, value).foldLeft(newState){ (state, _) => state.out(" ") }
        case None => newState
      }
    }
    val spaces = Word("spaces", "(n -- )", spacesfn)

    val emitfn = { (state: State) =>
      val (x, newState) = state.pop()
      x match {
        case Some(value) => newState.out(value.toChar.toString)
        case None => newState
      }
    }
    val emit = Word("emit", "(c -- )", emitfn)

    val cr = Word("cr", "( -- )", (state: State) => { state.out("\r\n") })

    val debug = Word("debug", "( -- )", (state: State) => { state.abort(s"DUMP: ${state.toString}") })

    Dictionary(Map[String, Word](
      interpret.load,
      compile.load,
      semicolon.load,
      number.load,
      execute.load,
      plus.load,
      minus.load,
      times.load,
      divide.load,
      dot.load,
      look.load,
      dup.load,
      see.load,
      exit.load,
      spaces.load,
      emit.load,
      cr.load,
      debug.load
    ))
  }
}
