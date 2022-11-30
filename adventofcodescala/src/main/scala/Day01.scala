import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters._

class Day01 extends Day {

  override val label: String = "01"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  override def taskZeroLogic(): String =
    getTop3 match
      case Some((max, _, _)) => max.toString
      case None => noSolutionFound

  override def taskOneLogic(): String =
    getTop3 match
      case Some((first, second, third)) => (first + second + third).toString
      case None => noSolutionFound

  private def getTop3: Option[(Int, Int, Int)] =
    val lines = input.linesIterator
    var list: ArrayBuffer[Int] = ArrayBuffer[Int]()
    val listOfList: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer[ArrayBuffer[Int]]()
    for (line <- lines) {
      if (line.isBlank) {
        listOfList += list
        list = ArrayBuffer[Int]()
      } else {
        line.toIntOption match
          case Some(x) => list += x
          case None =>
      }
    }

    listOfList
      .map({ ab => ab.sum })
      .sorted(Ordering.Int.reverse)
      .take(3)
      .toList match
      case x0 :: x1 :: x2 :: _ => Some((x0, x1, x2))
      case _ => None
}
