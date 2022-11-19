package sforth.model

import sforth.model.Data.DataItem

object State {
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
                   status: Status) {

    def abort: State = this.copy(status = Abort)
    def abort(message: String): State = {
      println(message)
      this.copy(status = Abort)
    }

    def stackUnderflow: State = {
      this.copy(status = StackUnderflow)
    }

    def look: State = {
      stack.headOption match {
        case None =>
          this.stackUnderflow
        case Some(dataItem: DataItem) =>
          println(s"${dataItem.item}\t{${this.stackSize}}")
          this
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
