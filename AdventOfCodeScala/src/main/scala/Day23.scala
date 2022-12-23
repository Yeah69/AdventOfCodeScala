import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day23 extends Day {
  private enum Direction:
    case North, South, West, East

  override val label: String = "23"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val directions: Array[Direction] =
    Seq(Direction.North, Direction.South, Direction.West, Direction.East).toArray

  private def getInitialPositions(): mutable.HashSet[(Int, Int)] =
    val set = mutable.HashSet[(Int, Int)]()
    val iterator = input.linesIterator
      .zipWithIndex
      .flatMap((line, y) => line.iterator.zipWithIndex.filter((c, _) => c == '#').map((_, x) => (x, y)))
    for (position <- iterator) {
      set.add(position)
    }
    set

  private def getNeighborTiles(position: (Int, Int)): Seq[(Int, Int)] =
    Seq((position._1 - 1, position._2 - 1), (position._1, position._2 - 1), (position._1 + 1, position._2 - 1),
      (position._1 - 1, position._2), (position._1 + 1, position._2),
      (position._1 - 1, position._2 + 1), (position._1, position._2 + 1), (position._1 + 1, position._2 + 1))

  private def getDirectedNeighborTiles(position: (Int, Int), direction: Direction): Seq[(Int, Int)] =
    direction match
      case Direction.North => Seq((position._1 - 1, position._2 - 1), (position._1, position._2 - 1), (position._1 + 1, position._2 - 1))
      case Direction.South => Seq((position._1 - 1, position._2 + 1), (position._1, position._2 + 1), (position._1 + 1, position._2 + 1))
      case Direction.West => Seq((position._1 - 1, position._2 - 1), (position._1 - 1, position._2), (position._1 - 1, position._2 + 1))
      case Direction.East => Seq((position._1 + 1, position._2 - 1), (position._1 + 1, position._2), (position._1 + 1, position._2 + 1))

  private def getDirectedPosition(position: (Int, Int), direction: Direction): (Int, Int) =
    direction match
      case Direction.North => (position._1, position._2 - 1)
      case Direction.South => (position._1, position._2 + 1)
      case Direction.West => (position._1 - 1, position._2)
      case Direction.East => (position._1 + 1, position._2)

  private def step(currentPositions: mutable.HashSet[(Int, Int)], i: Int): Unit =
    val moves = currentPositions
      // Filter for elves that have a non-empty neighbor tile
      .filter(p => getNeighborTiles(p).count(n => currentPositions.contains(n)) > 0)
      // Get move propositions
      .flatMap(p =>
        directions.indices
          // Order directions by task logic
          .map(j => (i + j) % directions.length)
          .map(j => directions(j))
          // Check whether neighbors are free into that direction
          .filter(d => getDirectedNeighborTiles(p, d).forall(n => !currentPositions.contains(n)))
          .map(d => (getDirectedPosition(p, d), p))
          .headOption)
      // Group move propositions by target
      .groupBy(_._1)
      // Filter for targets which were proposed once
      .filter((_, s) => s.count(_ => true) == 1)
      .map((t, s) => (t, s.head._2))

    for ((target, current) <- moves) {
      currentPositions.remove(current)
      currentPositions.add(target)
    }

  override def taskZeroLogic(): String =
    val currentPositions = getInitialPositions()

    for (i <- 0 until 10) {
      step(currentPositions, i)
    }

    val minX = currentPositions.map(_._1).min
    val maxX = currentPositions.map(_._1).max
    val minY = currentPositions.map(_._2).min
    val maxY = currentPositions.map(_._2).max

    (minX to maxX).flatMap(x => (minY to maxY).map(y => (x, y)))
      .count(p => !currentPositions.contains(p))
      .toString

  override def taskOneLogic(): String =
    val currentPositions = getInitialPositions()
    var i = 0

    while (currentPositions.count(p => getNeighborTiles(p).count(n => currentPositions.contains(n)) > 0) > 0) {
      step(currentPositions, i)
      i += 1
    }

    (i + 1).toString
}
