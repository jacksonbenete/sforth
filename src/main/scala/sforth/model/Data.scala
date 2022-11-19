package sforth.model

object Data {
  trait DataType
  object Number extends DataType
  object Literal extends DataType
  object Word extends DataType
  object Empty extends DataType
  object Invalid extends DataType

  case class DataItem(dataType: DataType, item: Any)
  object DataItem {
    def empty(): DataItem = DataItem(Empty, None)
  }
}
