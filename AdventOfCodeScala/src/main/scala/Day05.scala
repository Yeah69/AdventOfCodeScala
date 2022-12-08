import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.{Regex, UnanchoredRegex}

class Day05 extends Day {

  override val label: String = "05"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val lines: Array[String] = input.linesIterator.toArray
  private val splitRow: Int = lines.indexWhere(l => l.isBlank)
  private val indexToColumn: Map[Char, Int] = lines(splitRow - 1).iterator.zipWithIndex.filter((c, _) => c != ' ').toMap
  private def indexToStack(): Map[Char, mutable.Stack[Char]] =
    indexToColumn
      .map(kvp =>
        val stack = mutable.Stack[Char]()
        stack.pushAll((0 until splitRow - 1)
          .map(i => if lines(i).length > kvp._2 then lines(i)(kvp._2) else '%')
          .filter(c => c.isLetter)
          .reverse)
        (kvp._1, stack))
  private val instructionPattern: Regex = "move ([0-9]+) from ([0-9]) to ([0-9])".r
  private val instructions: Array[(Int, Char, Char)] =
    lines
      .drop(splitRow + 1)
      .map(line =>
        line match
          case instructionPattern(amount, from, to) => (amount.toInt, from(0), to(0))
          case _ => (0, '1', '2'))

  def taskLogic(modPop: scala.collection.Seq[Char] => scala.collection.Seq[Char]): String =
    val localIndexToStack = indexToStack()
    for ((amount, from, to) <- instructions) {
      var i = amount
      localIndexToStack(to).pushAll(
        modPop(localIndexToStack(from).popWhile(_ =>
          i -= 1
          i >= 0)))
    }
    String(localIndexToStack
      .map((char, stack) => (char, stack.pop()))
      .toList
      .sortWith(_._1 < _._1)
      .map((_, top) => top)
      .toArray)

  override def taskZeroLogic(): String = taskLogic(s => s)

  override def taskOneLogic(): String = taskLogic(s => s.reverse)
}
