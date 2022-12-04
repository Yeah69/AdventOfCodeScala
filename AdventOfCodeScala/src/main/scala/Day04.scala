import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day04 extends Day {

  override val label: String = "04"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val pairs = input
    .linesIterator
    .map({ line =>
      val pairs = line.split(',')
      val firstRange = pairs(0).split('-').map({ numText => numText.toInt })
      val secondRange = pairs(1).split('-').map({ numText => numText.toInt })
      ((firstRange(0), firstRange(1)), (secondRange(0), secondRange(1)))
    })
    .toArray
  
  private def taskLogic(predicate: (((Int, Int), (Int, Int))) => Boolean): String =
    pairs.count(predicate).toString

  private def firstContainsTheSecond(first: (Int, Int), second: (Int, Int)): Boolean =
    first._1 <= second._1 && first._2 >= second._2

  override def taskZeroLogic(): String = 
    taskLogic({pair => firstContainsTheSecond(pair._1, pair._2) || firstContainsTheSecond(pair._2, pair._1)})

  private def firstOverlapsTheSecond(first: (Int, Int), second: (Int, Int)): Boolean =
    first._2 >= second._1 && first._2 <= second._2 || first._1 >= second._1 && first._1 <= second._2

  override def taskOneLogic(): String =
    taskLogic({ pair => firstOverlapsTheSecond(pair._1, pair._2) || firstOverlapsTheSecond(pair._2, pair._1)})
}
