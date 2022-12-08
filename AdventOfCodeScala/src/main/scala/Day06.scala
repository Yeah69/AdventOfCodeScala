import scala.collection.immutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day06 extends Day {

  override val label: String = "06"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private def taskLogic(length: Int): String =
    (0 until input.length - length)
      .filter(i => HashSet.from((i until i + length).map(j => input(j))).count(_ => true) == length)
      .map(i => i + length)
      .head
      .toString

  override def taskZeroLogic(): String = taskLogic(4)

  override def taskOneLogic(): String = taskLogic(14)
}
