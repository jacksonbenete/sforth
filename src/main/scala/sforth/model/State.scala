package sforth.model

import sforth.model.Data.DataItem

object State {
  sealed trait IODevice
  object StdOutput extends IODevice

  case class IO(data: List[String], device: IODevice = StdOutput) {
    override def toString: String = data.mkString(" ")
  }
  object IO {
    def apply(data: String): IO = IO(List(data))
  }

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
                   stack: List[DataItem],
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
        case Some(dataItem: DataItem) => this.out(s"${dataItem.item}\t{${this.stackSize}}")
      }
    }

    def stackSize: Int = this.stack.size

    def push(value: DataItem): State = {
      this.copy(stack = value :: stack)
    }

    def pop(): (DataItem, State) = {
      this.stack.headOption match {
        case None => (DataItem.empty(), this.stackUnderflow)
        case Some(value) => (value, this.copy(stack = stack.tail))
      }
    }

    def take2(): (DataItem, DataItem, State) = {
      val (data1, state1) = pop()
      val (data2, state2) = state1.pop()
      state2.status match {
        case Abort => (DataItem.empty(), DataItem.empty(), this.abort)
        case StackUnderflow => (DataItem.empty(), DataItem.empty(), this.stackUnderflow)
        case _ => (data1, data2, state2)
      }
    }
  }
}
