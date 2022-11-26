package sforth.system

import sforth.model.DataStructures.Word
import sforth.model.State.State
import sforth.model.State.Status._

import java.time.LocalDateTime

case object NumberRunner {
  def apply(word: String): Option[Int] = word.toIntOption
}
