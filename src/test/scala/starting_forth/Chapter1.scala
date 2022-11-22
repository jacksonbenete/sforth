package starting_forth
import org.scalatest.funsuite.AnyFunSuite
import sforth.model.Data.{DataItem, Number}
import sforth.model.State.Status._
import utils.TestUtils._
import sforth.system.Interpreter

class Chapter1 extends AnyFunSuite{
  implicit val state = initialState
  implicit val subSystem = InterpreterSubSystem

  // spaces emit cr do-loop dot-quote
  test("pg.9 All This and ... Interactive!") {
    assert(interpreter("15 spaces").io.raw == "               ")
    assert(interpreter("15 spaces").stackSize == 0)

    assert(interpreter("42 emit").io.raw == "*")
    assert(interpreter("42 emit").stackSize == 0)

    assert(interpreter("15 spaces 42 emit 42 emit").io.raw == "               **")

    // words and numbers could be separate with as many spaces as wanted for clarity
    val addStar = compiler("star   42 emit")
    assert(addStar.dictionary.dict.size > state.dictionary.dict.size)
    assert(interpreter("  2  2  2  +  +  ").stack.head == DataItem(Number, 6))

    assert(interpreter("CR STAR CR STAR CR STAR")(addStar).io.raw == "\r\n*\r\n*\r\n*")

    val addMargin = compiler(": MARGIN CR 30 SPACES ;")(addStar)
    assert(
      interpreter("MARGIN STAR MARGIN STAR MARGIN STAR")(addMargin) ==
        interpreter("CR STAR CR STAR CR STAR")(addMargin))

    val addBlip = compiler(": BLIP   MARGIN STAR ;")(addMargin)
    val addStars = compiler(": STARS O DO STAR LOOP ;")(addBlip)
    assert(interpreter("5 stars")(addStars).io.raw == "*****")

    // Print F
    val addBar = compiler(": bar   margin 5 stars ;")(addStars)
    assert(
      interpreter("BAR BLIP BAR BLIP BLIP CR")(addBar).io.raw ==
      "*****\r\n*\r\n*****\r\n*\r\n*\r\n")
  }

  test("pg.14 The Dictionary") {
    // dot-quote is actually the ." word, to define a string
    val addGreet = compiler(": GREET .\" HELLO, I SPEAK FORTH \" ;")
    assert(addGreet.dictionary.dict.size > initialState.dictionary.dict.size)
    assert(interpreter("greet")(addGreet).io.raw == "HELLO, I SPEAK FORTH")
  }

  test("pg.19 The Stack: FORTH's Worksite for Arithmetic") {
    val addState = interpreter("3 4 +")
    assert(addState.stack.head == DataItem(Number, 7))
    assert(addState.stackSize == 1)

    val dot = interpreter("7 .")
    assert(dot.io.raw == "7")
    assert(dot.stackSize == 0)

    val composeWords = interpreter("3 4 + .")
    assert(composeWords.io.raw == "7")
    assert(composeWords.stackSize == 0)
  }

  test("pg.24 Keep Track of Your Stack") {
    val stackOrder = interpreter("2 4 6 8 . . . .")
    assert(stackOrder.io.raw == "8 6 4 2")
    assert(stackOrder.stackSize == 0)

    val stackUnderflow = interpreter(".")
    assert(stackUnderflow.io.raw == "Stack-underflow")
    assert(stackUnderflow.status == StackUnderflow)

    // glossary, or comments
    val glossary = compiler(": add (n1 n2 -- sum) + ;") // TODO
//    assert(interpreter("2 3 +").stack.head == interpreter("2 3 add")(glossary).stack.head)
//    assert(glossary.dictionary.dict.get("add").get.glossary == "(n1 n2 -- sum)")
  }

  test("pg.29 Problems - Chapter 1") {
    // 1. testing the compiler
    val addGift = compiler(": gift .\" Bookends \" ;")
    val addGiver = compiler(": giver .\" Stephanie \" ;")(addGift)
    val addThanks = compiler(": thanks .\" Dear \" giver .\" , \" cr 4 spaces .\" Thanks for the \" gift .\" . \" ;")(addGiver)
    assert(interpreter("thanks")(addThanks).io.raw == "Dear Stephanie,\r\n    Thanks for the Bookends.")

    // 2. subtract using +
    val thenLess = compiler(": then.less -10 + ;")
    assert(interpreter("11 then.less")(thenLess).stack.head == DataItem(Number, 1))

    // 3. redefining a word should be possible, but other words should retain previous definition
    val redefineGiver = compiler(": giver .\" Alphonse \" ;")(addThanks)
    // new definitions should be different
    assert(interpreter("giver")(redefineGiver).io.raw != interpreter("giver")(addThanks).io.raw)
    // but compiled word should retain previous definition
    assert(interpreter("thanks")(redefineGiver).io.raw == interpreter("thanks")(addThanks).io.raw)
  }

}
