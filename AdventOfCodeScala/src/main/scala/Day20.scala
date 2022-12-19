import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day20 extends Day {

  override val label: String = "20"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val numbers: Array[Long] = input.linesIterator.map(_.toLong).toArray

  private def getNodeBy(current: Node, by: Long): Node =
    var ret = current
    if by < 0 then
      for (_ <- -1L to by by -1L) {
        ret = ret.previous
      }
    else if by > 0 then
      for (_ <- 1L to by) {
        ret = ret.next
      }
    ret

  private def taskLogic(valueMod: Long => Long, loopCount: Int): String =
    val nodes = numbers.iterator.map(v =>
      val node = Node()
      node.value = valueMod(v)
      node)
      .toArray
    for (i <- nodes.indices) {
      val current = nodes(i)
      current.previous = nodes(if i == 0 then nodes.length - 1 else i - 1)
      current.next = nodes(if i == nodes.length - 1 then 0 else i + 1)
    }

    for (_ <- 0 until loopCount) {
      for (i <- nodes.indices) {
        val current = nodes(i)
        val currentPrevious = current.previous
        val currentNext = current.next
        currentPrevious.next = currentNext
        currentNext.previous = currentPrevious

        val length = nodes.length - 1
        var value = current.value
        while (value < 0) {
          value += length
        }
        value = value % length
        val minusValue = value - length

        val temp = if -minusValue < value then getNodeBy(currentPrevious, minusValue) else getNodeBy(currentPrevious, value)
        current.previous = temp
        current.next = temp.next
        current.previous.next = current
        current.next.previous = current
        println(s"Done $i")
      }
      println("One loop done")
    }

    val zero = nodes.filter(n => n.value == 0).head
    (getNodeBy(zero, 1000).value + getNodeBy(zero, 2000).value + getNodeBy(zero, 3000).value).toString

  override def taskZeroLogic(): String = taskLogic(v => v, 1)

  override def taskOneLogic(): String = taskLogic(v => v * 811589153L, 10)

  private class Node {
    var value: Long = _
    var previous: Node = _
    var next: Node = _
  }
}
