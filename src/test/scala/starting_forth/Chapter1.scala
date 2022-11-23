package starting_forth
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data.{Number}
import sforth.model.State.Status._
import utils.TestUtils._

class Chapter1 extends AnyFunSuite{
  implicit val state = initialState

  // spaces emit cr do-loop dot-quote
  test("pg.9 All This and ... Interactive!") {
    assert(Interpreter("15 spaces").output == "               ")
    assert(Interpreter("15 spaces").stackSize == 0)

    assert(Interpreter("42 emit").output == "*")
    assert(Interpreter("42 emit").stackSize == 0)

    assert(Interpreter("15 spaces 42 emit 42 emit").output == "               **")

    // words and numbers could be separate with as many spaces as wanted for clarity
    val addStar = Compiler("star   42 emit")
    assert(addStar.dictionary.dict.size > state.dictionary.dict.size)
    assert(Interpreter("  2  2  2  +  +  ").topStack == 6)

    assert(addStar.interpreter("CR STAR CR STAR CR STAR").output == "\r\n*\r\n*\r\n*")

    val addMargin = addStar.compiler(": MARGIN CR 30 SPACES ;")
    assert(
      addMargin.interpreter("MARGIN STAR MARGIN STAR MARGIN STAR") ==
        addMargin.interpreter("CR STAR CR STAR CR STAR"))

    val addBlip = addMargin.compiler(": BLIP   MARGIN STAR ;")
    val addStars = addBlip.compiler(": STARS O DO STAR LOOP ;")
    assert(addStars.interpreter("5 stars").output == "*****")

    // Print F
    val addBar = addStars.compiler(": bar   margin 5 stars ;")
    assert(
      addBar.interpreter("BAR BLIP BAR BLIP BLIP CR").output ==
      "*****\r\n*\r\n*****\r\n*\r\n*\r\n")
  }

  test("pg.14 The Dictionary") {
    // dot-quote is actually the ." word, to define a string
    val addGreet = Compiler(": GREET .\" HELLO, I SPEAK FORTH \" ;")
    assert(addGreet.dictionary.dict.size > initialState.dictionary.dict.size)
    assert(addGreet.interpreter("greet").output == "HELLO, I SPEAK FORTH")
  }

  test("pg.19 The Stack: FORTH's Worksite for Arithmetic") {
    val addState = Interpreter("3 4 +")
    assert(addState.topStack == 7)
    assert(addState.stackSize == 1)

    val dot = Interpreter("7 .")
    assert(dot.output == "7")
    assert(dot.stackSize == 0)

    val composeWords = Interpreter("3 4 + .")
    assert(composeWords.output == "7")
    assert(composeWords.stackSize == 0)
  }

  test("pg.24 Keep Track of Your Stack") {
    val stackOrder = Interpreter("2 4 6 8 . . . .")
    assert(stackOrder.output == "8 6 4 2")
    assert(stackOrder.stackSize == 0)

    val stackUnderflow = Interpreter(".")
    assert(stackUnderflow.output == "Stack-underflow")
    assert(stackUnderflow.status == StackUnderflow)

    // glossary, or comments
    val glossary = Compiler(": add (n1 n2 -- sum) + ;") // TODO
//    assert(Interpreter("2 3 +").topStack == Interpreter("2 3 add")(glossary).topStack)
//    assert(glossary.dictionary.dict.get("add").get.glossary == "(n1 n2 -- sum)")
  }

  test("pg.29 Problems - Chapter 1") {
    // 1. testing the Compiler
    val problem1 = Compiler(": gift .\" Bookends \" ;")
      .compiler(": giver .\" Stephanie \" ;")
      .compiler(": thanks .\" Dear \" giver .\" , \" cr 4 spaces .\" Thanks for the \" gift .\" . \" ;")
    assert(problem1.interpreter("thanks").output == "Dear Stephanie,\r\n    Thanks for the Bookends.")

    // 2. subtract using +
    val thenLess = Compiler(": then.less -10 + ;")
    assert(thenLess.interpreter("11 then.less").topStack == 1)

    // 3. redefining a word should be possible, but other words should retain previous definition
    val problem2 = problem1.compiler(": giver .\" Alphonse \" ;")
    // new definitions should be different
    assert(problem2.interpreter("giver").output != problem1.interpreter("giver").output)
    // but compiled word should retain previous definition
    assert(problem2.interpreter("thanks").output != problem1.interpreter("thanks").output)
  }

}
