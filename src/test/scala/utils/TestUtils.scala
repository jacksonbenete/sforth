package utils

import sforth.model.Dictionary
import sforth.model.State.Status._
import sforth.model.State.{IO, Registers, State}

object TestUtils {
  private val systemDictionary = Dictionary.systemDictionary
  private val stack = List[Int]()
  private val registers = Registers()
  private val mark = ">"
  private val namespace = Map[String, Dictionary]((mark, systemDictionary))
  private val input = List[String]()
  private val io = IO(List())
  private val status = Valid

  val initialState = State(systemDictionary, stack, registers, namespace, mark, input, io, status)

  def parseInput(string: String): List[String] = {
    string.toLowerCase.split(" ").toList.filterNot(_.isEmpty)
  }

  case class Engine(string: String, state: State) {
    def interpreter(string: String): Engine = Interpreter(string)(state)

    def compiler(string: String): Engine = Compiler(string)(state)

    def io: IO = this.state.io
    def output: String = this.state.io.raw.replace("\tok", "")
    def status: Status = this.state.status
    def stack: List[Int] = this.state.stack
    def topStack: Int = this.state.stack.head
    def dictionary: Dictionary = this.state.dictionary
    def stackSize: Int = this.state.stackSize

  }
  object Compiler {
    def apply(string: String)(implicit state: State = initialState): Engine = {
      val nextState = sforth.system.Compiler(state.compileMode.copy(input = parseInput(string)))
      Engine(string, nextState)
    }
  }
  object Interpreter {
    def apply(string: String)(implicit state: State = initialState): Engine = {
      val nextState = sforth.system.Interpreter(state.interpretMode.copy(input = parseInput(string)))
      Engine(string, nextState)
    }
  }
}
