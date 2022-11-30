import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day12 extends Day {

  override val label: String = "12"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private def getPosition(grid: Array[Array[Char]], seekedValue: Char): (Int, Int) =
    val y = grid.indexWhere(row => row.contains(seekedValue))
    (y, grid(y).indexWhere(c => c == seekedValue))

  private val data: ((Int, Int), (Int, Int), Array[Array[Int]]) =
    val temp = input.linesIterator
      .map(line => line.map(c => c).toArray)
      .toArray
    (getPosition(temp, 'S'), getPosition(temp, 'E'), temp.map(row => row.map(c => c match
      case 'S' => 0
      case 'E' => 'z'.toInt - 'a'.toInt
      case el => el.toInt - 'a'.toInt)))

  private def taskLogic(start: (Int, Int)): String =
    val (_, finish, elevationGrid) = data
    val score = elevationGrid.map(row => row.map(_ => Int.MaxValue))
    val queue = mutable.Queue[(Int, Int)]()
    queue.enqueue(start)
    score(start._1)(start._2) = 0
    while (queue.nonEmpty) {
      val (y, x) = queue.dequeue
      val elevation = elevationGrid(y)(x)
      val currScore = score(y)(x)

      for ((nextY, nextX) <- Seq((y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1))) {
        if (nextY >= 0 && nextY < elevationGrid.length
          && nextX >= 0 && nextX < elevationGrid(0).length) {
          val nextElevation = elevationGrid(nextY)(nextX)
          if (score(nextY)(nextX) == Int.MaxValue && nextElevation <= elevation + 1) {
            score(nextY)(nextX) = currScore + 1
            queue.enqueue((nextY, nextX))
          }
        }
      }
    }
    score(finish._1)(finish._2).toString

  override def taskZeroLogic(): String = taskLogic(data._1)

  override def taskOneLogic(): String =
    data._3
      .zipWithIndex
      .map((row, y) => row.zipWithIndex.filter((value, _) => value == 0).map((_, x) => (y, x)))
      .flatten
      .map(t => taskLogic(t).toInt)
      .min
      .toString
}
