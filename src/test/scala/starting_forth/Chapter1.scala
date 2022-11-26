package starting_forth
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data.{Number}
import sforth.model.State.Status._
import utils.TestUtils._

class Chapter1 extends AnyFunSuite{
  implicit val state = initialState

  // pg.9 All This and ... Interactive!
  test("testing basic functions consuming from stack") {
    assert(Interpreter("15 spaces").output == "               ")
    assert(Interpreter("15 spaces").stackSize == 0)

    assert(Interpreter("42 emit").output == "*")
    assert(Interpreter("42 emit").stackSize == 0)

    assert(Interpreter("15 spaces 42 emit 42 emit").output == "               **")
  }

  test("testing basic compiler version") {
    // words and numbers could be separate with as many spaces as wanted for clarity
    val addStar = Compiler(": star   42 emit ;")
    assert(addStar.dictionary.dict.size > state.dictionary.dict.size)
    assert(Interpreter("  2  2  2  +  +  ").topStack == 6)

    assert(addStar.interpreter("CR STAR CR STAR CR STAR").output == "\r\n*\r\n*\r\n*")

    val addMargin = addStar.compiler(": MARGIN CR 30 SPACES ;")
    assert(addMargin.interpreter("MARGIN STAR").output == "\r\n                              *")
  }

  test("testing advanced compiler version (do loop)") {
    val doLoop = Compiler(": star   42 emit ;")
      .compiler(": MARGIN CR 30 SPACES ;")
      .compiler(": BLIP   MARGIN STAR ;")
      .compiler(": STARS 0 DO STAR LOOP ;")
    assert(doLoop.interpreter("5 stars").output == "*****")

    // Print F
    val addBar = doLoop.compiler(": bar   margin 5 stars ;")
    val f1 = "\r\n                              *****"
    val f2 = "\r\n                              *"
    val f3 = "\r\n                              *****"
    val f4 = "\r\n                              *"
    val f5 = "\r\n                              *\r\n"
    assert(addBar.interpreter("BAR BLIP BAR BLIP BLIP CR").output == f1 ++ f2 ++ f3 ++ f4 ++ f5)
  }

  // pg.14 The Dictionary
  test("implement dot-quote") {
    // dot-quote is actually the ." word, to define a string
    val addGreet = Compiler(": GREET .\" HELLO, I SPEAK FORTH \" ;")
    assert(addGreet.dictionary.dict.size > initialState.dictionary.dict.size)
    assert(addGreet.interpreter("greet").output == "HELLO, I SPEAK FORTH")
  }

  // pg.19 The Stack: FORTH's Worksite for Arithmetic
  test("testing stack implementation") {
    val addState = Interpreter("3 4 +")
    assert(addState.topStack == 7)
    assert(addState.stackSize == 1)

    val dot = Interpreter("7 .")
    assert(dot.output == "7 ")
    assert(dot.stackSize == 0)

    val composeWords = Interpreter("3 4 + .")
    assert(composeWords.output == "7 ")
    assert(composeWords.stackSize == 0)
  }

  // pg.24 Keep Track of Your Stack
  test("testing stack output order and underflow") {
    val stackOrder = Interpreter("2 4 6 8 . . . .")
    assert(stackOrder.output == "8 6 4 2 ")
    assert(stackOrder.stackSize == 0)

    val stackUnderflow = Interpreter(".")
    // NOTE: REPL do prints "Stack-underflow", but not the Interpreter
    //assert(stackUnderflow.output == "Stack-underflow")
    assert(stackUnderflow.status == StackUnderflow)
  }

  test("testing glossary implementation") {
    // Compiler should be able to either ignore or store comments inside parens
    val glossary = Compiler(": add (n1 n2 -- sum) + ;")
    assert(glossary.interpreter("2 3 +").topStack == Interpreter("2 3 add").topStack)
    assert(glossary.dictionary.dict.get("add").get.glossary == "(n1 n2 -- sum)")
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
