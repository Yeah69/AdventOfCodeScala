import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day18 extends Day {

  override val label: String = "18"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val data: Set[(Int, Int, Int)] =
    input.linesIterator
      .map(line =>
        val parts = line.split(",")
        (parts(0).toInt, parts(1).toInt, parts(2).toInt))
      .toSet

  private def getNeighbors(cube: (Int, Int, Int)): Seq[(Int, Int, Int)] = Seq(
    (cube._1 - 1, cube._2, cube._3),
    (cube._1 + 1, cube._2, cube._3),
    (cube._1, cube._2 - 1, cube._3),
    (cube._1, cube._2 + 1, cube._3),
    (cube._1, cube._2, cube._3 - 1),
    (cube._1, cube._2, cube._3 + 1))

  private def countTotalSurfaceArea(set: Set[(Int, Int, Int)]): Int = set
    .iterator
    .flatMap(getNeighbors)
    .count(neighbor => !set.contains(neighbor))

  private val totalSurfaceArea = countTotalSurfaceArea(data)

  override def taskZeroLogic(): String = totalSurfaceArea.toString

  override def taskOneLogic(): String =
    val minX = data.iterator.map(_._1).min
    val maxX = data.iterator.map(_._1).max
    val minY = data.iterator.map(_._2).min
    val maxY = data.iterator.map(_._2).max
    val minZ = data.iterator.map(_._3).min
    val maxZ = data.iterator.map(_._3).max

    val outerSet: mutable.HashSet[(Int, Int, Int)] = mutable.HashSet[(Int, Int, Int)]()
    val outerQueue = mutable.Queue[(Int, Int, Int)]((minX - 1, minY - 1, minZ - 1))
    while (outerQueue.nonEmpty) {
      val current = outerQueue.dequeue()
      outerSet.add(current)
      for (next <- getNeighbors(current)
        .filter(neighbor => neighbor._1 >= minX - 1 && neighbor._1 <= maxX + 1
          && neighbor._2 >= minY - 1 && neighbor._2 <= maxY + 1
          && neighbor._3 >= minZ - 1 && neighbor._3 <= maxZ + 1)
        .filter(neighbor => !data.contains(neighbor) && !outerSet.contains(neighbor))) {
        outerSet.add(next)
        outerQueue.enqueue(next)
      }
    }

    val innerSet: mutable.HashSet[(Int, Int, Int)] = mutable.HashSet[(Int, Int, Int)]()
    for (x <- minX to maxX; y <- minY to maxY; z <- minZ to maxZ) {
      val coordinates = (x, y, z)
      if !outerSet.contains(coordinates) && !data.contains(coordinates) then
        innerSet.add(coordinates)
    }

    (totalSurfaceArea - countTotalSurfaceArea(innerSet.toSet)).toString
}
