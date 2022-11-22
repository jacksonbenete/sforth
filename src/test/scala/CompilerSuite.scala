import utils.TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.Status.Valid

class CompilerSuite extends AnyFunSuite {
  implicit val state = initialState

  test("define square word") {
    assert(Compiler("square dup *").dictionary.dict.size > state.dictionary.dict.size)
  }

  test("composing words to test TestEngine") {
    val stateResult = Compiler(": square dup * ;").compiler(": cube square square ;").interpreter("3 cube")
    assert(stateResult.topStack == DataItem(Number, 81))
  }
}
