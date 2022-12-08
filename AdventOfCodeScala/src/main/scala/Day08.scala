import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day08 extends Day {

  override val label: String = "08"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  val grid: Array[Array[Int]] =
    val lines = input.linesIterator.toArray
    val ret = Array.ofDim[Int](input.linesIterator.count(_ => true), lines.head.length)
    for (y <- lines.indices) {
      for (x <- lines(y).indices) {
        ret(y)(x) = lines(y)(x).toInt
      }
    }
    ret

  override def taskZeroLogic(): String =
    val visibilityGrid = Array.ofDim[Boolean](grid.length, grid.head.length)
    for (y <- grid.indices) {
      var max = -1
      for (x <- grid(y).indices) {
        val current = grid(y)(x)
        if current > max then
          visibilityGrid(y)(x) = true
          max = current
      }
      max = -1
      for (x <- grid(y).indices.reverse) {
        val current = grid(y)(x)
        if current > max then
          visibilityGrid(y)(x) = true
          max = current
      }
    }
    for (x <- grid.head.indices) {
      var max = -1
      for (y <- grid.indices) {
        val current = grid(y)(x)
        if current > max then
          visibilityGrid(y)(x) = true
          max = current
      }
      max = -1
      for (y <- grid.indices.reverse) {
        val current = grid(y)(x)
        if current > max then
          visibilityGrid(y)(x) = true
          max = current
      }
    }
    visibilityGrid.flatten.count(b => b).toString

  override def taskOneLogic(): String =
    var max = -1
    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
        val value = grid(y)(x)
        var left = 0
        var otherX = x - 1
        var break = false
        while (otherX >= 0 && !break) {
          val otherValue = grid(y)(otherX)
          if otherValue <= value then
            left += 1
          if otherValue >= value then
            break = true
          otherX -= 1
        }
        var right = 0
        otherX = x + 1
        break = false
        while (otherX < grid(y).length && !break) {
          val otherValue = grid(y)(otherX)
          if otherValue <= value then
            right += 1
          if otherValue >= value then
            break = true
          otherX += 1
        }
        
        var up = 0
        var otherY = y - 1
        break = false
        while (otherY >= 0 && !break) {
          val otherValue = grid(otherY)(x)
          if otherValue <= value then
            up += 1
          if otherValue >= value then
            break = true
          otherY -= 1
        }
        var down = 0
        otherY = y + 1
        break = false
        while (otherY < grid.length && !break) {
          val otherValue = grid(otherY)(x)
          if otherValue <= value then
            down += 1
          if otherValue >= value then
            break = true
          otherY += 1
        }
        val result = left * right * up * down
        if result > max then max = result
      }
    }
    max.toString
}
