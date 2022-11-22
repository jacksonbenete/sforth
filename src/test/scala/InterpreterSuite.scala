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

  test("look should return the top of the stack and the stack size as a string") {
    assert(mockInput("2 .s").io.data.mkString == "2\t{1}")
    assert(mockInput("2 .s .s").io.data.mkString == "2\t{1}2\t{1}")
  }

  test("pop and other printing functions should successfully produce output") {
    assert(mockInput("2 .").io.data.mkString == "2\t{0}")
    assert(mockInput("2 2 2 . . .").io.data.mkString == "2\t{2}2\t{1}2\t{0}")
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
