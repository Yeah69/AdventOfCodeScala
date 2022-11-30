import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day11 extends Day {

  override val label: String = "11"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val numberPattern: Regex = "Monkey (\\d+):".r
  private val startingItemsPattern: Regex = " {2}Starting items: (.+)".r
  private val operationPattern: Regex = " {2}Operation: new = (.+)".r
  private val moduloPattern: Regex = " {2}Test: divisible by (\\d+)".r
  private val trueCasePattern: Regex = " {4}If true: throw to monkey (\\d+)".r
  private val falseCasePattern: Regex = " {4}If false: throw to monkey (\\d+)".r

  private def getMonkeys: Array[Monkey] =
    val monkeys: Array[Monkey] = input.split(sys.props("line.separator") + sys.props("line.separator"))
      .map(monkeyText =>
        val lines = monkeyText.linesIterator.toArray
        val number = lines(0) match
          case numberPattern(numberText) => numberText.toInt
          case _ => -1
        val startingItems = lines(1) match
          case startingItemsPattern(listText) =>
            val queue = mutable.Queue[Long]()
            for (item <- listText.split(", ").map(t => t.toLong)) {
              queue.enqueue(item)
            }
            queue
          case _ => mutable.Queue()
        val operationText = lines(2) match
          case operationPattern(opText) => opText
          case _ => "0 + 0"
        val operationParts = operationText.split(' ')
        val firstOperand = operationParts(0) match
          case "old" => None
          case t => Some(t.toLong)
        val operator = operationParts(1)(0)
        val secondOperand = operationParts(2) match
          case "old" => None
          case t => Some(t.toLong)
        val modulo = lines(3) match
          case moduloPattern(modText) => modText.toLong
          case _ => 0
        val trueCaseNumber = lines(4) match
          case trueCasePattern(num) => num.toInt
          case _ => -1
        val falseCaseNumber = lines(5) match
          case falseCasePattern(num) => num.toInt
          case _ => -1
        Monkey(number, startingItems, firstOperand, operator, secondOperand, modulo, trueCaseNumber, falseCaseNumber))
    val monkeyIndex = monkeys.map(m => (m.getNumber, m)).toMap
    val lowestCommonMultiple = monkeys.map(m => m.getModulo).product
    for (monkey <- monkeys) {
      monkey.setThrowingTargets(monkeyIndex)
      monkey.lowestCommonMultiple = lowestCommonMultiple
    }
    monkeys

  private def taskLogic(roundCount: Int, monkeyRoundFunction: Monkey => Unit): String =
    val monkeys = getMonkeys
    for (_ <- 0 until roundCount) {
      for (monkey <- monkeys) {
        monkeyRoundFunction(monkey)
      }
    }
    monkeys.map(m => m.inspectionCounter).sorted(Ordering.Long.reverse).take(2).product.toString

  override def taskZeroLogic(): String = taskLogic(20, m => m.roundForTaskZero())

  override def taskOneLogic(): String = taskLogic(10000, m => m.roundForTaskOne())

  private class Monkey(number: Int, items: mutable.Queue[Long], firstOperand: Option[Long], operation: Char, secondOperand: Option[Long], modulo: Long, trueCaseMonkeyNumber: Int, falseCaseMonkeyNumber: Int) {
    private var trueCaseMonkey: Monkey = _
    private var falseCaseMonkey: Monkey = _
    var inspectionCounter: Long = 0L
    var lowestCommonMultiple = 0L

    def getNumber: Int = number
    def getModulo: Long = modulo
    def setThrowingTargets(map: Map[Int, Monkey]): Unit = {
      trueCaseMonkey = map(trueCaseMonkeyNumber)
      falseCaseMonkey = map(falseCaseMonkeyNumber)
    }

    private def enqueueItem(item: Long): Unit = items.enqueue(item)

    def roundForTaskZero(): Unit = round(worryLevel => (worryLevel.toDouble / 3.0).toLong)

    def roundForTaskOne(): Unit = round(worryLevel => worryLevel % lowestCommonMultiple)

    private def round(worryLevelMod: Long => Long): Unit = {
      while (items.nonEmpty) {
        val old = items.dequeue()
        val firstActualOperand = firstOperand match
          case Some(value) => value
          case _ => old
        val secondActualOperand = secondOperand match
          case Some(value) => value
          case _ => old
        val newValue = operation match
          case '+' => firstActualOperand + secondActualOperand
          case '*' => firstActualOperand * secondActualOperand
          case _ => 0L
        val worryLevel = worryLevelMod(newValue)
        if worryLevel % modulo == 0 then trueCaseMonkey.enqueueItem(worryLevel)
        else falseCaseMonkey.enqueueItem(worryLevel)
        inspectionCounter += 1L
      }
    }
  }
}
