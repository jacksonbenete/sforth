package sforth.model

case class Word(name: String, function: State => DataItem)

case class Dictionary(dict: Map[String, Word])
