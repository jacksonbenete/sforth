import TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.Status.Valid

class IntegrationTests extends AnyFunSuite {
  implicit val state = initialState

  test("integration: define word in compiler and eval on interpreter") {
    val compiler1 = mockInput(": square dup * exit ;")(state, CompilerSubSystem)
    val eval1 = mockInput("3 square")(compiler1, InterpreterSubSystem)
    assert(eval1.stack.head == DataItem(Number, 9))
    val compiler2 = mockInput(": cube square square ;")(compiler1, CompilerSubSystem)
    val eval2 = mockInput("3 cube")(compiler2, InterpreterSubSystem)
    assert(eval2.stack.head == DataItem(Number, 81))
  }
}
