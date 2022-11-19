package tsforth

import tsforth.DataStructures._


object Compiler {
  println("SForth Compiler Version: 0.1")
  def apply[R](state: State[R]): State[R] = {
    // TODO: currently not accepting redefining word that already exist
    // TODO: breaks compiler in future version if input doesn't ends with semicolon
    if (!state.input.endsWith(";")) println(s"WARNING: compiler needs `;` (semicolon) terminate definition symbol.")

    // create a new dictionary entry with newWorld that returns a function that executes every word in the array
    val (newWorldList, definitionList) = state.input.replaceFirst(": ", "").replaceFirst(";$", "").split(" ").toList.splitAt(1)
    val newWorldName = newWorldList.head

    // first validate if all words on definition exists
    val validateWords = definitionList.foldLeft((true, "", List[Word[R]]())) {
      (stillValid: Tuple3[Boolean, String, List[Word[R]]], word: String) =>
        val (stillValidFlag, invalidWord, validWordList) = stillValid
        (stillValidFlag, state.dictionary(word)) match {
          case (false, _) => (false, invalidWord, validWordList)
          case (true, None) => (false, word, validWordList)
          case (true, Some(validWord)) => (true, "", validWord :: validWordList)
        }
    }
    validateWords match {
      case (false, invalidWord, _) =>
        println(s"Word $invalidWord is undefined.")
        state.copy(specialFlag = Abort())
      case (true, _, validWordList) =>
        val fnDefinition = (state: State[R]) => {
          // foldLeft would evaluate in the wrong order
          validWordList.foldRight(state) { (word: Word[R], nextState: State[R]) =>
            word.function(nextState)
          }
        }
        val newWord = Word(newWorldName, fnDefinition)
        val newDictionary = Map[String, Word[R]]((newWorldName, newWord)) ++ state.dictionary.dict
        state.copy(dictionary = Dictionary(newDictionary))
    }
  }
}
