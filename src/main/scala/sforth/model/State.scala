package sforth.model

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

  case class Registers(counterLoop: Int = 0)

  object Status {
    sealed trait Status
    object Abort extends Status
    object Exit extends Status
    object Valid extends Status
    object Failure extends Status
    object StackUnderflow extends Status
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
    def out(message: String) = this.copy(io = IO(message :: this.io.data))

    def abort: State = this.copy(status = Abort)
    def abort(message: String): State = this.copy(status = Abort, io = IO(message))

    def stackUnderflow: State = {
      this.copy(status = StackUnderflow)
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
