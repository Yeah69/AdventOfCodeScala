import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day19 extends Day {

  override val label: String = "19"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val blueprintPattern: Regex = "Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.".r

  private val blueprints: Array[(Int, Int, Int, (Int, Int), (Int, Int))] =
    input.linesIterator
      .map(line => line match
        case blueprintPattern(numberT, oreT, clayT, obsidianZeroT, obsidianOneT, geodeZeroT, geodeOneT) =>
          (numberT.toInt, oreT.toInt, clayT.toInt, (obsidianZeroT.toInt, obsidianOneT.toInt), (geodeZeroT.toInt, geodeOneT.toInt))
        case _ => (-1, -1, -1, (-1, -1), (-1, -1)))
      .toArray

  private def addTuple(left: (Int, Int, Int, Int), right: (Int, Int, Int, Int)) =
    (left._1 + right._1, left._2 + right._2, left._3 + right._3, left._4 + right._4)

  private def getOptions(blueprintNumber: Int, robots: (Int, Int, Int, Int), resources: (Int, Int, Int, Int)): Seq[((Int, Int, Int, Int), (Int, Int, Int, Int))] =
    val ret: ArrayBuffer[((Int, Int, Int, Int), (Int, Int, Int, Int))] = ArrayBuffer[((Int, Int, Int, Int), (Int, Int, Int, Int))]()
    val blueprint = blueprints(blueprintNumber - 1)
    val oreRobot = blueprint._2 <= resources._1
      && (robots._1 < blueprint._2 || robots._1 < blueprint._3 || robots._1 < blueprint._4._1 || robots._1 < blueprint._5._1)
    val clayRobot = blueprint._3 <= resources._1 && robots._2 < blueprint._4._2
    val obsidianRobot = blueprint._4._1 <= resources._1 && blueprint._4._2 <= resources._2 && robots._3 < blueprint._5._2
    val geodeRobot = blueprint._5._1 <= resources._1 && blueprint._5._2 <= resources._3
    if !obsidianRobot && !geodeRobot then
      ret.addOne((robots, addTuple(resources, robots)))
    if !obsidianRobot && !geodeRobot && oreRobot then
      val newResources = addTuple(addTuple(resources, (-blueprint._2, 0, 0, 0)), robots)
      val newRobots = addTuple(robots, (1, 0, 0, 0))
      ret.addOne((newRobots, newResources))
    if !obsidianRobot && !geodeRobot && clayRobot then
      val newResources = addTuple(addTuple(resources, (-blueprint._3, 0, 0, 0)), robots)
      val newRobots = addTuple(robots, (0, 1, 0, 0))
      ret.addOne((newRobots, newResources))
    if !geodeRobot && obsidianRobot then
      val newResources = addTuple(addTuple(resources, (-blueprint._4._1, -blueprint._4._2, 0, 0)), robots)
      val newRobots = addTuple(robots, (0, 0, 1, 0))
      ret.addOne((newRobots, newResources))
    if geodeRobot then
      val newResources = addTuple(addTuple(resources, (-blueprint._5._1, 0, -blueprint._5._2, 0)), robots)
      val newRobots = addTuple(robots, (0, 0, 0, 1))
      ret.addOne((newRobots, newResources))
    //if ret.isEmpty then
    //  ret.addOne((robots, addTuple(resources, robots)))
    ret.toSeq

  private def getMaxOpenedGeodes(blueprintNumber: Int, robots: (Int, Int, Int, Int), resources: (Int, Int, Int, Int), minutesLeft: Int): Int =
    if minutesLeft == 0 then
      return resources._4
    val newMinutesLeft = minutesLeft - 1
    val max = getOptions(blueprintNumber, robots, resources)
      .map((robs, reses) => getMaxOpenedGeodes(blueprintNumber, robs, reses, newMinutesLeft))
      .max
    max

  override def taskZeroLogic(): String =
    blueprints
      .iterator
      .map((bn, _, _, _, _) =>
        println(s"Doing $bn")
        val max = getMaxOpenedGeodes(bn, (1, 0, 0, 0), (0, 0, 0, 0), 24)
        println(s"Max $max")
        bn * max)
      .sum
      .toString

  override def taskOneLogic(): String =
    blueprints
      .iterator
      .take(3)
      .map((bn, _, _, _, _) =>
        println(s"Doing $bn")
        val max = getMaxOpenedGeodes(bn, (1, 0, 0, 0), (0, 0, 0, 0), 32)
        println(s"Max $max")
        max)
      .product
      .toString
}
