package sforth.system

import sforth.model.DataStructures.Word
import sforth.model.Dictionary
import sforth.model.State._

import scala.annotation.tailrec


object Compiler {
  trait CompilerStatus
  object Valid extends CompilerStatus
  object Invalid extends CompilerStatus

  def apply(state: State): State = {
    case class CompilerState(input: List[String], invalidWord: String, validWordList: List[Word], status: CompilerStatus)

    // validate if all words in definition are valid
    @tailrec
    def validateDefinition(compilerState: CompilerState): CompilerState = {
      compilerState match {
        case CompilerState(_, _, _, Invalid) => compilerState
        case CompilerState(Nil, _, _, Valid) => compilerState
        case CompilerState(word :: tail, _, validWordList, Valid) => state.dictionary(word) match {
          case Some(validWord) => validateDefinition(CompilerState(tail, "", validWord :: validWordList  ,Valid))
          case None => validateDefinition(CompilerState(Nil, word, validWordList, Invalid))
        }
      }
    }

    val (newWordName, definition) = state.input.filterNot(_ == ":").filterNot(_ == ";").splitAt(1)

    validateDefinition(CompilerState(definition, "", List(), Valid)) match {
      case CompilerState(_, invalidWord, _, Invalid) => state.abort(s"Word $invalidWord undefined")
      case CompilerState(_, _, validWordList, Valid) =>
        // create lambda from reverse definition word list
        val newWordFunction = (state: State) => {
          validWordList.foldRight(state) { (word: Word, nextState: State) =>
            word.function(nextState)
          }
        }
        // return new state with new word and updated dictionary
        val newWord = Word(newWordName.head, newWordFunction)
        val newDictionary = Map[String, Word]((newWord.name, newWord)) ++ state.dictionary.dict
        state.copy(dictionary = Dictionary(newDictionary))
    }
  }
}
