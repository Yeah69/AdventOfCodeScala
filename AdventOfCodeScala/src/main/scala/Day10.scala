import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day10 extends Day {

  override val label: String = "10"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val noopPattern: Regex = "noop".r
  private val addxPattern: Regex = "addx (.+)".r

  val instructions: Array[Int] =
    input.linesIterator
      .flatMap(line =>
        line match
          case noopPattern() => Array(0)
          case addxPattern(valueText) => Array(0, valueText.toInt))
      .toArray

  override def taskZeroLogic(): String =
    var x = 1
    var sum = 0
    for (i <- 1 to 220) {
      if (i - 20) % 40 == 0 then
        sum += i * x
      x = step(x, i - 1)
    }
    sum.toString

  override def taskOneLogic(): String =
    var x = 1
    for (row <- 0 until 6) {
      for (column <- 0 until 40) {
        val i = row * 40 + column
        print(if Math.abs(x - column) <= 1 then '█' else ' ')
        x = step(x, i)
      }
      println()
    }
    "PAPJCBHP"

  private def step(x: Int, i: Int): Int =
    x + instructions(i)
}
