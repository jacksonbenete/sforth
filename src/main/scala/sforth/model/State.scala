package sforth.model

import sforth.model.DataStructures.Word

object State {
  sealed trait IODevice
  object StdOutput extends IODevice

  case class IO(data: List[String], device: IODevice = StdOutput) {
    override def toString: String = data.mkString(" ")
    def raw: String = this.data.mkString
    def debug: Unit = println(s">>>>\tIO data: ${this.data.mkString}\n>>>>\tIO device: ${this.device}")
  }
  object IO {
    def apply(data: String): IO = IO(List(data))
  }

  case class Registers(counter: Int = 0,
                       input: String = "",
                       word: Word = Word("", "", state => state)) {

    override def toString: String =
      s"""
         |\tCounter Register: ${this.counter},
         |\tInput Register: ${this.input},
         |\tWord Register: ${this.word.name}
         |""".stripMargin

    def input(string: String): Registers = this.copy(input = string)
    def word(word: Word): Registers = this.copy(word = word)
  }

  object Status {
    sealed trait Status
    object Abort extends Status
    object Exit extends Status
    object Valid extends Status
    object NotANumber extends Status
    object Failure extends Status // if an intermediate step would allow recovery
    object StackUnderflow extends Status
    object InterpretMode extends Status
    object CompileMode extends Status
    object Loop extends Status
  }

  import sforth.model.State.Status._

  case class State(dictionary: Dictionary,
                   stack: List[Int],
                   registers: Registers,
                   namespace: Map[String, Dictionary],
                   mark: String, // mark current namespace
                   input: List[String],
                   io: IO,
                   status: Status) {

    override def toString: String =
      s"""
         |Status: ${this.status}
         |Stack: ${this.stack}
         |Input: ${this.input}
         |Registers: ${this.registers}
         |""".stripMargin

    def input(list: List[String]): State = this.copy(input = list)
    def out(message: String) = this.copy(io = IO(message :: this.io.data))

    // Set Status
    def abort: State = this.copy(status = Abort)
    def abort(message: String): State = this.copy(status = Abort, io = IO(message))
    def stackUnderflow: State = this.copy(status = StackUnderflow)
    def interpretMode: State = this.copy(status = InterpretMode)
    def compileMode: State = this.copy(status = CompileMode)
    def valid: State = this.copy(status = Valid)

    // clean state (e.g. reverse output list)
    def cleanState: State = {
      this.copy(io = IO(this.io.data.filter(_.nonEmpty).reverse),
        registers = Registers())
    }

    def look: State = {
      stack.headOption match {
        case None =>
          this.stackUnderflow
        case Some(data) => this.out(s"$data\t{${this.stackSize}}")
      }
    }

    def stackSize: Int = this.stack.size

    def push(value: Option[Int]): State = {
      value match {
        case Some(value) => this.copy(stack = value :: stack)
        case None => this.abort
      }
    }

    def pop(): (Option[Int], State) = this.stack.headOption match {
      case None => (None, this.stackUnderflow)
      case value => (value, this.copy(stack = stack.tail))
    }

    def take2(): (Option[Int], Option[Int], State) = {
      if (this.stackSize < 2) {
        val empty = None: Option[Int]
        (empty, empty, this.stackUnderflow)
      } else {
        val (data1, state1) = pop()
        val (data2, state2) = state1.pop()
        (data1, data2, state2)
      }
    }
  }
}
