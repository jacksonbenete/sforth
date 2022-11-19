package sforth.model

case class DataItem(item: Tuple2[DataType, Any])

trait DataType
case class Number() extends DataType
case class Literal() extends DataType
case class Word() extends DataType
