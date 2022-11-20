import TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.Status.Valid

class CompilerSuite extends AnyFunSuite {
  implicit val state = initialState
  implicit val subSystem = CompilerSubSystem

  test("define square word") {
    assert(mockInput("square dup *").dictionary.dict.size > state.dictionary.dict.size)
  }
}
