import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day14 extends Day {

  override val label: String = "14"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val stones: Set[(Int, Int)] =
    input.linesIterator
      .flatMap(line =>
        val lineCoordinates = line.split(" -> ")
          .map(part =>
            val coordinates = part.split(",")
            (coordinates(0).toInt, coordinates(1).toInt))
        lineCoordinates.zip(lineCoordinates.drop(1))
          .flatMap((from, to) =>
            if from._1 == to._1 then
              (Math.min(from._2, to._2) to Math.max(from._2, to._2)).map(i => (from._1, i))
            else if from._2 == to._2 then
              (Math.min(from._1, to._1) to Math.max(from._1, to._1)).map(i => (i, from._2))
            else IndexedSeq()))
      .toSet


  private def taskLogic(isOneTask: Boolean): String =
    val restingSand = mutable.HashSet[(Int, Int)]()
    val max = stones.map(_._2).max + 1
    var running = true
    var i = 0
    while (running && !restingSand.contains((500, 0))) {
      var currentPos = (500, 0)
      var moved = true
      while (moved) {
        val nextPos = Seq(
          (currentPos._1, currentPos._2 + 1),
          (currentPos._1 - 1, currentPos._2 + 1),
          (currentPos._1 + 1, currentPos._2 + 1))
          .find(pos => !stones.contains(pos) && !restingSand.contains(pos))
        nextPos match
          case Some(pos) => currentPos = pos
          case None =>
            moved = false
            i += 1
            restingSand.add(currentPos)
        if currentPos._2 == max then
          moved = false
          if isOneTask then
            i += 1
            restingSand.add(currentPos)
          else
            running = false
      }
    }
    i.toString

  override def taskZeroLogic(): String = taskLogic(false)

  override def taskOneLogic(): String = taskLogic(true)
}
