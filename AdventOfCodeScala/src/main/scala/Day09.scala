import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day09 extends Day {

  override val label: String = "09"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val instructionPattern: Regex = "([UDLR]) (\\d+)".r

  private val instructions: Array[(Char, Int)] =
    input.linesIterator
      .map(line =>
        line match
          case instructionPattern(directionText, lengthText) => (directionText.head, lengthText.toInt)
          case _ => ('U', 0))
      .toArray

  def taskLogic(number: Int): String =
    val set: mutable.HashSet[(Int, Int)] = mutable.HashSet[(Int, Int)]()
    val knots: Array[(Int, Int)] = (0 until number).map(_ => (0, 0)).toArray
    set.add(knots.last)
    for ((dir, length) <- instructions) {
      for (_ <- 0 until length) {
        dir match
          case 'U' => knots(0) = (knots.head._1, knots.head._2 + 1)
          case 'D' => knots(0) = (knots.head._1, knots.head._2 - 1)
          case 'R' => knots(0) = (knots.head._1 + 1, knots.head._2)
          case 'L' => knots(0) = (knots.head._1 - 1, knots.head._2)

        for (i <- 0 until (number - 1)) {
          val validTailPositions =
            surroundingBlock(knots(i)).toSet
          if !validTailPositions.contains(knots(i + 1)) then
            val possibleTailPositions =
              surroundingBlock(knots(i + 1)).toSet
            val intersection = validTailPositions.intersect(possibleTailPositions)
            knots(i + 1) = intersection
              .minBy((x, y) => Math.abs(knots(i)._1 - x) + Math.abs(knots(i)._2 - y))
        }

        set.add(knots.last)
      }
    }
    set.count(_ => true).toString

  private def surroundingBlock(pos: (Int, Int)): Seq[(Int, Int)] =
    for (x <- pos._1 - 1 to pos._1 + 1; y <- pos._2 - 1 to pos._2 + 1) yield (x, y)

  override def taskZeroLogic(): String = taskLogic(2)

  override def taskOneLogic(): String = taskLogic(10)
}
