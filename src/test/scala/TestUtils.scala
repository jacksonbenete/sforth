import sforth.model.Data.DataItem
import sforth.model.Dictionary
import sforth.model.State.State
import sforth.model.State.Status.Valid
import sforth.system.REPL.REPL
import sforth.system.{Compiler, Interpreter}

object TestUtils {
  private val systemDictionary = Dictionary.systemDictionary
  private val stack = List[DataItem]()
  private val mark = ">"
  private val namespace = Map[String, Dictionary]((mark, systemDictionary))
  private val input = List[String]()
  private val status = Valid

  sealed trait SubSystem
  object CompilerSubSystem extends SubSystem
  object InterpreterSubSystem extends SubSystem
  object REPLSubSystem extends SubSystem

  val initialState = State(systemDictionary, stack, namespace, mark, input, status)

  def mockInput(string: String)(implicit state: State, subSystem: SubSystem): State = {
    val input = string.split(" ").toList

    subSystem match {
      case CompilerSubSystem => Compiler(state.copy(input = input))
      case InterpreterSubSystem => Interpreter(state.copy(input = input))
      case REPLSubSystem => REPL(state.copy(input = input))
    }
  }
}
