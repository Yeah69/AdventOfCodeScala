import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day03 extends Day {

  override val label: String = "03"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  override def taskZeroLogic(): String =
    input
      .linesIterator
      .map(line =>
        val count = line.count({ _ => true })
        val (first, second) = line.splitAt(count / 2)
        val secondSet = second.toSet
        val matched = first
          .iterator
          .filter({ c => secondSet.contains(c)})
          .take(1)
          .toList
          .head
        getPriority(matched))
      .sum
      .toString

  override def taskOneLogic(): String =
    input
      .linesIterator
      .grouped(3)
      .map({ group =>
        val first = group.head
        val secondSet = group(1).toSet
        val thirdSet = group(2).toSet
        var ret = 0
        for (c <- first) 
          if secondSet.contains(c) && thirdSet.contains(c) then ret = getPriority(c)
        ret
      })
      .sum
      .toString

  private def getPriority(itemType: Char): Int =
    if itemType > 96 then itemType - 96 else itemType - 38
}
