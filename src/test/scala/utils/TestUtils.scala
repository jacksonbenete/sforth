package utils

import sforth.model.Data.DataItem
import sforth.model.Dictionary
import sforth.model.State.Status._
import sforth.model.State.{IO, State}

object TestUtils {
  private val systemDictionary = Dictionary.systemDictionary
  private val stack = List[DataItem]()
  private val mark = ">"
  private val namespace = Map[String, Dictionary]((mark, systemDictionary))
  private val input = List[String]()
  private val io = IO(List())
  private val status = Valid

  val initialState = State(systemDictionary, stack, namespace, mark, input, io, status)

  case class Engine(string: String, state: State) {
    def interpreter(string: String): Engine = {
      val nextState = sforth.system.Interpreter(state.copy(input = string.split(" ").toList))
      Engine(string, nextState)
    }

    def compiler(string: String): Engine = {
      val nextState = sforth.system.Compiler(state.copy(input = string.split(" ").toList))
      Engine(string, nextState)
    }

    def io: IO = this.state.io
    def status: Status = this.state.status
    def stack: List[DataItem] = this.state.stack
    def topStack: DataItem = stack.head
    def dictionary: Dictionary = this.state.dictionary

  }
  object Compiler {
    def apply(string: String)(implicit state: State = initialState): Engine = {
      val nextState = sforth.system.Compiler(state.copy(input = string.split(" ").toList))
      Engine(string, nextState)
    }
  }
  object Interpreter {
    def apply(string: String)(implicit state: State = initialState): Engine = {
      val nextState = sforth.system.Interpreter(state.copy(input = string.split(" ").toList))
      Engine(string, nextState)
    }
  }
}
