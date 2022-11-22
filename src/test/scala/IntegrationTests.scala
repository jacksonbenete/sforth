import utils.TestUtils._
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data._
import sforth.model.State.Status.Valid

class IntegrationTests extends AnyFunSuite {
  implicit val state = initialState

  test("integration: define word in compiler and eval on interpreter") {
    val result1 = Compiler(": square dup * exit ;").interpreter("3 square")
    assert(result1.topStack == DataItem(Number, 9))

    val result2 = Compiler(": cube square square ;").interpreter("3 cube")
    assert(result2.topStack == DataItem(Number, 81))
  }
}
