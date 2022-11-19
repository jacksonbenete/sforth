package tsforth

object DataStructures {
  case class Word[R](key: String, function: State[R] => State[R])

  case class Dictionary[R](dict: Map[String, Word[R]]) {
    def apply(key: String): Option[Word[R]] = {
      dict.get(key)
    }
  }

  trait InterpreterFlag
  case class Abort() extends InterpreterFlag
  case class Exit() extends InterpreterFlag
  case class ValidState() extends InterpreterFlag
  case class StackUnderflow() extends InterpreterFlag
  case class Failure() extends InterpreterFlag


  trait StackType
  case class IntStack() extends StackType
  case class StringStack() extends StackType
  case class WordStack() extends StackType

  case class State[R](dictionary: Dictionary[R],          // map of words and associate functions
                      namespace: Map[String, Dictionary[R]], // define with `mark` to later rewind the dictionary
                      //namespaceStack: List[String],          // list of marks
                      stackPointer: List[StackType],         // define the type found on top of stack
                      stackInt: List[Int],                   // stack of integers for type-safety
                      stackWord: List[Word[R]],              // stack of words for type-safety
                      stackString: List[String],             // stack of strings for type-safety
                      stackSize: Int,
                      input: String,                         // current/last input received
                      specialFlag: InterpreterFlag
                     ) {

    def head(): Option[Any] = { stackPointer match {
      case Nil => None
      case IntStack() :: pointerTail => Option(stackInt.head)
      case e =>
        println(s"head not implemented for type ${e.getClass}")
        Option(Failure())
    }
    }
    def pop(): (Option[Any], State[R]) = { stackPointer match {
      case Nil => {
        (None, this)
      }
      case IntStack() :: pointerTail => {
        val (i, intTail) = stackInt.splitAt(1)
        (Option(i.head), this.copy(stackInt = intTail, stackPointer = pointerTail, stackSize = stackSize - 1))
      }
      case e => {
        println(s"pop not implemented for type ${e.getClass}")
        (None, this)
      }
    }
    }

    def push(elem: Any): State[R] = {
      elem match {
        case elem: Int => {
          val newState = this.copy(stackInt = elem :: stackInt, stackPointer = IntStack() :: stackPointer, stackSize = stackSize + 1)
          //        println(s"DEBUG State: $newState")
          newState
        }
        case e => {
          println(s"push not implemented for type ${e.getClass}")
          this
        }
      }
    }
  }

}
