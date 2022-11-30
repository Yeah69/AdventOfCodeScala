import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day24 extends Day {
  private enum Direction:
    case Left, Right, Up, Down

  override val label: String = "24"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val blizzards: Map[(Int, Int), Direction] =
    input.linesIterator
      .zipWithIndex
      .flatMap((line, y) => line.iterator
        .zipWithIndex
        .filter((c, _) => c == '>' || c == '<' || c == '^' || c == 'v')
        .map((c, x) => ((x - 1, y - 1), c match
          case '<' => Direction.Left
          case '>' => Direction.Right
          case '^' => Direction.Up
          case _ => Direction.Down)))
      .toMap

  private val cache: mutable.Map[Int, Set[(Int, Int)]] = mutable.Map[Int, Set[(Int, Int)]]()

  private val width = input.linesIterator.toArray.head.length - 2
  private val height = input.linesIterator.count(_ => true) - 2
  private val start = (0, -1)
  private val finish = (input.linesIterator.toArray.head.length - 3, input.linesIterator.count(_ => true) - 2)

  private def trip(initialSteps: Int, currentStart: (Int, Int), currentFinish: (Int, Int)): Int =
    val queue = mutable.Queue[(Int, (Int, Int))]()
    queue.enqueue((initialSteps, currentStart))

    while (queue.nonEmpty) {
      val (currentSteps, currentPosition) = queue.dequeue()
      if currentPosition == currentFinish then
        return currentSteps
      val nextSteps = currentSteps + 1

      val newBlizzards = if cache.contains(nextSteps) then cache(nextSteps) else
        val set = blizzards.iterator.map((bp, d) => d match
            case Direction.Right => ((bp._1 + nextSteps) % width, bp._2)
            case Direction.Left => ((bp._1 + width * 50 - nextSteps) % width, bp._2)
            case Direction.Down => (bp._1, (bp._2 + nextSteps) % height)
            case Direction.Up => (bp._1, (bp._2 + height * 50 - nextSteps) % height))
          .toSet
        cache(nextSteps) = set
        set

      val nextPos = Seq(currentPosition,
        (currentPosition._1 - 1, currentPosition._2),
        (currentPosition._1 + 1, currentPosition._2),
        (currentPosition._1, currentPosition._2 - 1),
        (currentPosition._1, currentPosition._2 + 1))
        .filter(p =>
          p == start
            || p == finish
            || p._1 >= 0 && p._1 < width
            && p._2 >= 0 && p._2 < height)
        .filter(p => !newBlizzards.contains(p))
      for (next <- nextPos) {
        if !queue.contains((nextSteps, next)) then
          queue.enqueue((nextSteps, next))
      }
    }

    Int.MaxValue

  override def taskZeroLogic(): String = trip(0, start, finish).toString

  override def taskOneLogic(): String =
    val first = trip(0, start, finish)
    val second = trip(first, finish, start)
    val third = trip(second, start, finish)
    third.toString
}
