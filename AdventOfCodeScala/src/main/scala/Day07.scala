import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day07 extends Day {

  override val label: String = "07"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  override def taskZeroLogic(): String = noSolutionFound

  override def taskOneLogic(): String = noSolutionFound
}
