import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day21 extends Day {

  override val label: String = "21"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val monkeys: Map[String, (String, Char, String)|Long] =
    input.linesIterator
      .map(line =>
        val generalParts = line.split(": ")
        val label = generalParts(0)
        if generalParts(1)(0).isDigit then
          (label, generalParts(1).toLong)
        else
          val parts = generalParts(1).split(' ')
          (label, (parts(0), parts(1)(0), parts(2))))
      .toMap

  private def calculate(currentMonkey: String): Long =
    monkeys(currentMonkey) match
      case i: Long => i
      case (leftMonkey, op, rightMonkey): (String, Char, String) =>
        val left = calculate(leftMonkey)
        val right = calculate(rightMonkey)
        op match
          case '+' => left + right
          case '-' => left - right
          case '*' => left * right
          case '/' => left / right
          case _ => 0 // only possible if input isn't parsed correctly

  override def taskZeroLogic(): String =
    calculate("root").toString

  private def humanHereQuestionMark(currentMonkey: String): Boolean =
    if currentMonkey == "humn" then true
    else
      monkeys(currentMonkey) match
        case _: Long => false
        case (leftMonkey, _, rightMonkey): (String, Char, String) =>
          humanHereQuestionMark(leftMonkey) || humanHereQuestionMark(rightMonkey)

  private def chooseSides(leftMonkey: String, rightMonkey: String): (String, String) =
    val monkeySide = if humanHereQuestionMark(leftMonkey) then rightMonkey else leftMonkey
    val humanSide = if monkeySide == leftMonkey then rightMonkey else leftMonkey
    (monkeySide, humanSide)

  private def calculateToTarget(currentMonkey: String, target: Long): Long =
    monkeys(currentMonkey) match
      case i: Long => i // shouldn't be possible
      case (leftMonkey, op, rightMonkey): (String, Char, String) =>
        val (monkeySide, humanSide) = chooseSides(leftMonkey, rightMonkey)
        val monkeySideValue = calculate(monkeySide)
        val newTarget = op match
          case '+' => target - monkeySideValue
          case '-' => if leftMonkey == monkeySide then monkeySideValue - target else target + monkeySideValue
          case '*' => target / monkeySideValue
          case '/' => if leftMonkey == monkeySide then monkeySideValue / target else target * monkeySideValue
          case _ => 0L // only possible if input isn't parsed correctly
        if humanSide == "humn" then newTarget
        else calculateToTarget(humanSide, newTarget)

  override def taskOneLogic(): String =
    monkeys("root") match
      case _: Long => noSolutionFound
      case (leftMonkey, _, rightMonkey): (String, Char, String) =>
        val (monkeySide, humanSide) = chooseSides(leftMonkey, rightMonkey)
        val target = calculate(monkeySide)
        calculateToTarget(humanSide, target).toString
}
