import utils.TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.Status.Valid

class IntegrationTests extends AnyFunSuite {
  implicit val state = initialState

  test("integration: define word in compiler and eval on interpreter") {
    assert(Compiler(": square dup * ;")
        .interpreter("3 square")
        .topStack == 9)

    assert(Compiler(": square dup * ;")
      .compiler(": cube square square ;")
      .interpreter("3 cube")
    .topStack == 81)
  }
}
