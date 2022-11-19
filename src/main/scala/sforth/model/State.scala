package sforth.model

trait Status
object Abort extends Status
object Exit extends Status
object Valid extends Status
object Failure extends Status
object StackUnderflow extends Status

case class State(dictionary: Dictionary,
                 stack: List[DataItem],
                 namespace: Map[String, Dictionary],
                 mark: String,                        // mark current namespace
                 input: String,
                 status: Status)
