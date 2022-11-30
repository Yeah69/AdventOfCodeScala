import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day25 extends Day {

  override val label: String = "25"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString
  private val snafuNumbers: Array[String] = input
    .linesIterator
    .toArray

  private def snafuDigitToDecimal(snafu: Char): Long =
    snafu match
      case '2' => 2L
      case '1' => 1L
      case '0' => 0L
      case '-' => -1L
      case _ => -2L

  private def addSnafuDigits(left: Char, right: Char, carriage: Char): (Char, Char) =
    val lDecimal = snafuDigitToDecimal(left)
    val rDecimal = snafuDigitToDecimal(right)
    val cDecimal = snafuDigitToDecimal(carriage)
    val resultDecimal = lDecimal + rDecimal + cDecimal
    resultDecimal match
      case -6 => ('-', '-')
      case -5 => ('-', '0')
      case -4 => ('-', '1')
      case -3 => ('-', '2')
      case -2 => ('0', '=')
      case -1 => ('0', '-')
      case 0 => ('0', '0')
      case 1 => ('0', '1')
      case 2 => ('0', '2')
      case 3 => ('1', '=')
      case 4 => ('1', '-')
      case 5 => ('1', '0')
      case 6 => ('1', '1')
      case _ => ('0', '0') // shouldn't happen

  private def addSnafu(left: String, right: String): String =
    val maxLength = Math.max(left.length, right.length)
    val lReverse = left.reverse.padTo(maxLength, '0')
    val rReverse = right.reverse.padTo(maxLength, '0')
    var carriage = '0'
    var ret = ""
    for (i <- 0 until maxLength) {
      val (newCarriage, newDigit) = addSnafuDigits(lReverse(i), rReverse(i), carriage)
      ret += newDigit
      carriage = newCarriage
    }
    if carriage != '0' then ret += carriage
    ret.reverse

  override def taskZeroLogic(): String =
    var ret = "0"
    for (snafu <- snafuNumbers) {
      ret = addSnafu(ret, snafu)
    }
    ret

  override def taskOneLogic(): String = nothingToDoHere
}
