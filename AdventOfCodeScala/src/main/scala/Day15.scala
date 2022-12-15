import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day15 extends Day {

  override val label: String = "15"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val linePattern: Regex = "Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)".r

  private val data: Array[((Int, Int), Int)] =
    input.linesIterator
      .map(line => line match
        case linePattern(sxText, syText, bxText, byText) => ((sxText.toInt, syText.toInt), (bxText.toInt, byText.toInt))
        case _ => ((0, 0), (0, 0)))
      .map((s, b) => (s, Math.abs(s._1 - b._1) + Math.abs(s._2 - b._2)))
      .toArray

  private def getRangesForRow(row: Int): Array[(Int, Int)] =
    data
      .map((s, r) =>
        val delta = r - Math.abs(s._2 - row)
        (s._1 - delta, s._1 + delta))
      .filter(r => r._1 <= r._2)

  private def getSortedInfo(ranges: Array[(Int, Int)]): Array[(Int, Boolean)] =
    ranges
      .flatMap(r => Seq((r._1, true), (r._2, false)))
      .sortBy(r => r)(Ordering.by[(Int, Boolean), Int](_._1).orElse(Ordering.by[(Int, Boolean), Boolean](_._2).reverse))

  private def getMergedRanges(row: Int): ArrayBuffer[(Int, Int)] =
    val rawRanges = getRangesForRow(row)
    val sortedInfo = getSortedInfo(rawRanges)
    val ret: ArrayBuffer[(Int, Int)] = ArrayBuffer()
    var currentStartX = -1
    var openTrueCount = 0
    for (curr <- sortedInfo) {
      if curr._2 then
        if openTrueCount == 0 then
          currentStartX = curr._1
        openTrueCount += 1
      else
        openTrueCount -= 1
        if openTrueCount == 0 then
          ret.addOne((currentStartX, curr._1))
    }
    ret

  override def taskZeroLogic(): String =
    getMergedRanges(2000000)
      .map(r => r._2 - r._1)
      .sum
      .toString

  override def taskOneLogic(): String =
    var ret = noSolutionFound
    var i = 0
    while (i <= 4000000 && ret == noSolutionFound) {
      val ranges = getMergedRanges(i)
      if ranges.length == 2 then
        ret = ((ranges(0)._2.toLong + 1L) * 4000000L + i.toLong).toString
      i += 1
    }
    ret
}
