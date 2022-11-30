import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day16 extends Day {

  override val label: String = "16"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val linePattern: Regex = "Valve (.+) has flow rate=(.+); (tunnels lead to valves|tunnel leads to valve) (.+)".r

  private val adjacencyMap: Map[String, (Int, Array[String])] =
    input.linesIterator
      .map(line => line match
        case linePattern(label, rateText, _, connections) => (label, (rateText.toInt, connections.split(", ")))
        case _ => ("ASDF", (-1, Array.empty[String])))
      .toMap
  private val mainNodes = adjacencyMap.filter((_, v) => v._1 > 0).map((l, _) => l).toSet
  private val paths = mainNodes
    .toArray
    .appended("AA")
    .map(m => (m, getShortestPaths(m, mainNodes)))
    .toMap

  private def getShortestPaths(startLabel: String, mainSet: Set[String]): Map[String, Int] =
    val doneNodes = mutable.HashSet[String]()
    val queue = mutable.Queue((startLabel, 1 /* one minute cost of releasing the valve */))
    val result = mutable.HashMap[String, Int]()
    while (queue.nonEmpty) {
      val (currentLabel, costs) = queue.dequeue()
      result.addOne((currentLabel, costs))
      doneNodes.add(currentLabel)
      for (next <- adjacencyMap(currentLabel)._2.filter(n => !doneNodes.contains(n))) {
        queue.addOne((next, costs + 1))
      }
    }
    result.filter((l, _) => mainSet.contains(l)).toMap

  private def getMaxPressureReleaseZero(currentLabel: String, amount: Int, minutesLeft: Int, doneSet: Set[String]): Int =
    val newDoneSet = doneSet.iterator.concat(Seq(currentLabel)).toSet
    paths(currentLabel)
      .filter((l, c) => !newDoneSet.contains(l) && minutesLeft - c > 0)
      .map((l, c) => getMaxPressureReleaseZero(
        l,
        amount + (minutesLeft - c) * adjacencyMap(l)._1,
        minutesLeft - c,
        newDoneSet))
      .maxOption match
      case Some(m) => m
      case None => amount

  override def taskZeroLogic(): String =
    getMaxPressureReleaseZero("AA", 0, 30, Set.empty[String]).toString

  private def getMaxPressureReleaseOne(currentLabel: (String, String), amount: Int, minutesLeft: (Int, Int), doneSet: mutable.Stack[String]): Int =
    val newMinutesLeft = Math.max(minutesLeft._1, minutesLeft._2)
    if minutesLeft._1 > minutesLeft._2 then
      paths(currentLabel._1)
        .filter((l, c) => !doneSet.contains(l) && newMinutesLeft - c > 0)
        .map((l, c) =>
          doneSet.push(l)
          val ret = getMaxPressureReleaseOne(
            (l, currentLabel._2),
            amount + (newMinutesLeft - c) * adjacencyMap(l)._1,
            (newMinutesLeft - c, minutesLeft._2),
            doneSet)
          doneSet.pop()
          ret)
        .maxOption match
        case Some(m) => m
        case None => amount
    else if minutesLeft._2 > minutesLeft._1 || currentLabel._1 == currentLabel._2 then
      paths(currentLabel._2)
        .filter((l, c) => !doneSet.contains(l) && newMinutesLeft - c > 0)
        .map((l, c) =>
          doneSet.push(l)
          val ret = getMaxPressureReleaseOne(
            (currentLabel._1, l),
            amount + (newMinutesLeft - c) * adjacencyMap(l)._1,
            (minutesLeft._1, newMinutesLeft - c),
            doneSet)
          doneSet.pop()
          ret)
        .maxOption match
        case Some(m) => m
        case None => amount
    else if currentLabel._1 != currentLabel._2 then
      paths(currentLabel._2)
        .filter((l, c) => !doneSet.contains(l) && newMinutesLeft - c > 0)
        .map((l, c) =>
          doneSet.push(l)
          val ret = getMaxPressureReleaseOne(
            (currentLabel._1, l),
            amount + (newMinutesLeft - c) * adjacencyMap(l)._1,
            (minutesLeft._1, newMinutesLeft - c),
            doneSet)
          doneSet.pop()
          ret)
        .concat(paths(currentLabel._1)
          .filter((l, c) => !doneSet.contains(l) && newMinutesLeft - c > 0)
          .map((l, c) =>
            doneSet.push(l)
            val ret = getMaxPressureReleaseOne(
              (l, currentLabel._2),
              amount + (newMinutesLeft - c) * adjacencyMap(l)._1,
              (newMinutesLeft - c, minutesLeft._2),
              doneSet)
            doneSet.pop()
            ret))
        .maxOption match
        case Some(m) => m
        case None => amount
    else amount
  override def taskOneLogic(): String =
    getMaxPressureReleaseOne(("AA", "AA"), 0, (26, 26), mutable.Stack[String]("AA")).toString
}
