import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day13 extends Day {

  override val label: String = "13"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private def textToListType(text: String, start: Int): (ListType, Int) =
    var i = start
    val list = ArrayBuffer[ListType|Int]()
    while (i < text.length) {
      i += 1
      text(i) match
        case '[' =>
          val (newList, newI) = textToListType(text, i)
          list.addOne(newList)
          i = newI
        case ']' =>
          return (ListType(list.toArray), i)
        case ',' =>
        case _ =>
          val endIndex = Seq('[',']',',').map(c => text.indexOf(c, i)).filter(v => v > -1).min
          list.addOne(text.substring(i, endIndex).toInt)
          i = endIndex - 1
    }
    (ListType(Array[ListType|Int]()), 0)

  private val pairs: Array[(ListType, ListType)] =
    input.split(sys.props("line.separator") + sys.props("line.separator"))
      .map(pairText =>
        val parts = pairText.split(sys.props("line.separator"))
        (textToListType(parts(0), 0)._1, textToListType(parts(1), 0)._1))

  override def taskZeroLogic(): String = pairs
    .map(pair => isRightOrder(pair._1, pair._2))
    .zipWithIndex
    .filter(_._1)
    .map(_._2 + 1)
    .sum
    .toString

  private def isRightOrder(left: ListType, right: ListType): Boolean =
    isRightOrderRec(left, right) match
      case Some(b) => b
      case None => true

  private def isRightOrderRec(left: ListType, right: ListType): Option[Boolean] =
    var i = 0
    var result: Option[Boolean] = None
    val max = Math.max(left.value.length, right.value.length)
    while (result.isEmpty && i < max) {
      result = if i >= left.value.length then Some(true)
        else if i >= right.value.length then Some(false)
        else None
      if (result.isEmpty)
        result = (left.value(i), right.value(i)) match
          case (l: Int, r: Int) => if l < r then Some(true) else if l > r then Some(false) else None
          case (l: Int, r: ListType) => isRightOrderRec(ListType(Array(l)), r)
          case (l: ListType, r: Int) => isRightOrderRec(l, ListType(Array(r)))
          case (l: ListType, r: ListType) => isRightOrderRec(l, r)
      i += 1
    }
    result

  override def taskOneLogic(): String =
    val two = ListType(Array(ListType(Array(2))))
    val six = ListType(Array(ListType(Array(6))))
    pairs.flatMap(pair => Seq(pair._1, pair._2))
      .appended(two)
      .appended(six)
      .sortWith(isRightOrder)
      .zipWithIndex
      .filter(t => t._1 == two || t._1 == six)
      .map(_._2 + 1)
      .product
      .toString

  private class ListType(inner: Array[ListType|Int]) {
    val value: Array[ListType|Int] = inner
  }
}
