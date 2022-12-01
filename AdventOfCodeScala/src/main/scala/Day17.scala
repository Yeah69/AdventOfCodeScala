import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day17 extends Day {

  override val label: String = "17"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  override def taskZeroLogic(): String = noSolutionFound

  override def taskOneLogic(): String = noSolutionFound
}
