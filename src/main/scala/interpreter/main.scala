package interpreter

import scala.io.StdIn.readLine

trait ValidTypes
case class ForthInt(value: Int) extends ValidTypes
case class ForthString(value: String) extends ValidTypes

trait Word[R] {
  val name: String
  val function: () => R
}
case class SystemWord[R](name: String, function: () => R) extends Word[R]
case class UserWord[R](name: String, function: () => R) extends Word[R]

case class Dictionary[R](dictionary: Map[String, Word[R]])

case class State[R](dictionary: Dictionary[R],
                      namespaces: Map[String, Dictionary[R]],
                      namespaceStack: List[String],
                      input: String,
                      stack: List[ValidTypes])

object main {

  println(s"Starting SForth...")

  val systemDictionary = Dictionary[ValidTypes](
    Map(
      ("+" -> SystemWord[ValidTypes]("+", {() =>
        state.stack match {
          case List(x: ForthInt, y: ForthInt, _) => x.value + y.value
          case _ => println(s">>> Not enough elements on stack.")
        }}))
    ))

  val state = State[ValidTypes](
    stack = List[ValidTypes](),
    dictionary = systemDictionary[ValidTypes],
    namespaces = Map[String, Dictionary[ValidTypes]]("default", systemDictionary),
    namespaceStack = List[String]("default"),
    input = ""
  )

  println(s"SForth Version: 0.1")

  def interpreter[R](state: State[R]): State[R] = {
    print(s"> ")
    val definition = readLine().split(" ")
    definition.map { word: String =>
      word match {
        case "exit" => println("goodbye")
        case "." =>
          state.stack.headOption match {
            case Some(value) => println(s"$value\t${state.stack.length}") // TODO: remove element
            case None => println(s"${state.stack.length}")
          }
          interpreter(state)
        case lookup =>
          // see if word exists
          state.dictionary.dictionary.get(word) match {
            case Some(value) => value.function()
            case None =>
              // if word isn't on dict, try to parse it as a ValidType and put on Stack
              lookup.toIntOption match {
                case Some(value) => ForthInt(value) :: state.stack
                case None => ForthString(lookup) :: state.stack
              }
          }
          interpreter(state)
      }
    }
    println(s"Dump state: ${state.toString}")
    state
  }

  def main(args: Array[String]): Unit = {
    interpreter(state)
  }
}
