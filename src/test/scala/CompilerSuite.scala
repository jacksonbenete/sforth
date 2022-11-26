import utils.TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.Status.{Abort, Valid}

class CompilerSuite extends AnyFunSuite {
  implicit val state = initialState

  test("define square word") {
    assert(Compiler(": square dup * ;").dictionary.dict.size > state.dictionary.dict.size)
  }

  test("composing words to test TestEngine") {
    val stateResult = Compiler(": square dup * ;").compiler(": cube square square ;").interpreter("3 cube")
    assert(stateResult.topStack == 81)
  }

  test("compiler should abort while parsing undefined word") {
    assert(Compiler(": foo bar ;").dictionary.dict.size == state.dictionary.dict.size)
    assert(Compiler(": foo bar ;").status == Abort)
  }
}
