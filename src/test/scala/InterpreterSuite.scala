import utils.TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.State
import sforth.model.State.Status.{StackUnderflow, Valid}

class InterpreterSuite extends AnyFunSuite{
  implicit val state = initialState

  test("a default initialState should be valid") {
    assert(Interpreter("")(state).state.status == Valid)
  }

  test("look should return the top of the stack and the stack size as a string") {
    assert(Interpreter("2 .s").io.raw == "2\t{1}")
    assert(Interpreter("2 .s .s").io.raw == "2\t{1}2\t{1}")
  }

  test("pop and other printing functions should successfully produce output") {
    assert(Interpreter("2 .").io.raw == "2\t{0}")
    assert(Interpreter("2 2 2 . . .").io.raw == "2\t{2}2\t{1}2\t{0}")
  }

  test("eval a word with empty stack should return StackUnderflow status") {
    assert(Interpreter("+").status == StackUnderflow)
    assert(Interpreter(".").status == StackUnderflow)
    assert(Interpreter(".s").status == StackUnderflow)
    assert(Interpreter("2 +").status == StackUnderflow)
  }

  test("exit should exit right away") {
    assert(Interpreter("2 2 2 + +").topStack == 6)
    assert(Interpreter("2 2 + exit 2 +").topStack != 6)
  }
}
