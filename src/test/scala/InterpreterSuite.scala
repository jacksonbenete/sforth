import TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.State
import sforth.model.State.Status.{StackUnderflow, Valid}
import sforth.system.Interpreter

class InterpreterSuite extends AnyFunSuite{
  implicit val state = initialState
  implicit val subSystem = InterpreterSubSystem

  test("a default initialState should be valid") {
    assert(Interpreter(state).status == Valid)
  }

  test("eval a word with empty stack should return StackUnderflow status") {
    assert(mockInput("+").status == StackUnderflow)
    assert(mockInput(".").status == StackUnderflow)
    assert(mockInput(".s").status == StackUnderflow)
    assert(mockInput("2 +").status == StackUnderflow)
  }

  test("exit should exit right away") {
    assert(mockInput("2 2 2 + +").stack.head == DataItem(Number, 6))
    assert(mockInput("2 2 + exit 2 +").stack.head != DataItem(Number, 6))
  }
}
