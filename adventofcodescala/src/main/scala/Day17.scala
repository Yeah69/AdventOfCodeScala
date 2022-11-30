import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day17 extends Day {

  override val label: String = "17"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private def step(i: Int, highest: Int, instructionPointer: Int, nopes: mutable.HashSet[(Int, Int)]): (Int, Int) =
    var myHighest = highest
    var myInstructionPointer = instructionPointer
    val shape = i % 5 match
      case 0 => Minus()
      case 1 => Plus()
      case 2 => MirroredL()
      case 3 => I()
      case 4 => Block()

    var currentPosition = (2, myHighest + 4)
    var falling = true
    while (falling) {
      val horizontalShift =
        if input(myInstructionPointer % input.length) == '<' then (currentPosition._1 - 1, currentPosition._2)
        else (currentPosition._1 + 1, currentPosition._2)
      myInstructionPointer += 1
      if !shape.collides(horizontalShift, nopes) then
        currentPosition = horizontalShift
      val verticalShift = (currentPosition._1, currentPosition._2 - 1)
      if shape.collides(verticalShift, nopes) then
        nopes.addAll(shape.getPositions(currentPosition))
        falling = false
        myHighest = Math.max(myHighest, currentPosition._2 + shape.highestOffset)
        for (nope <- nopes.iterator.filter((_, y) => y < myHighest - 50)) {
          nopes.remove(nope)
        }
      else
        currentPosition = verticalShift
    }
    //printState(nopes)
    (myInstructionPointer, myHighest)

  override def taskZeroLogic(): String =
    val nopes = mutable.HashSet[(Int, Int)]()
    var highest = -1
    var instructionPointer = 0
    for (i <- 0 until 2022) {
      val t = step(i, highest, instructionPointer, nopes)
      highest = t._2
      instructionPointer = t._1
    }
    (highest + 1).toString

  private def seekLoop(heights: ArrayBuffer[Int]): Option[(Int, Int)] =
    if heights.length <= 4 then return None
    val offset = heights.length % 2
    val loopLength = heights.length / 2
    val secondOffset = offset + loopLength
    val hasLoop = (0 until loopLength)
      .forall(i =>
        val first = heights(offset + i)
        val second = heights(secondOffset + i)
        first == second)
    val hasDifferentNumbers = (0 to loopLength).map(i => heights(offset + i)).toSet.count(_ => true) > 1
    if hasLoop && hasDifferentNumbers then Some((offset, loopLength)) else None

  private def getLoopInformation(): (ArrayBuffer[Int], Int, Int) =
    val nopes = mutable.HashSet[(Int, Int)]()
    var highest = -1
    var instructionPointer = 0
    val heigths = ArrayBuffer[Int]()
    var i = 0
    var loopResult: Option[(Int, Int)] = None
    while (loopResult.isEmpty) {
      for (_ <- 0 until input.length) {
        val t = step(i, highest, instructionPointer, nopes)
        highest = t._2
        instructionPointer = t._1
        i += 1
      }
      if heigths.isEmpty then heigths.addOne(highest + 1) else heigths.addOne(highest + 1 - heigths.sum)
      loopResult = seekLoop(heigths)
    }
    loopResult match
      case Some((offset, loopIterations)) => (heigths, offset, loopIterations)
      case None => (ArrayBuffer(), 0, 0)

  private def getRemainingHeightDiff(offsetIterationCount: Int, loopIterationsCount: Int, remainingStones: Int) =
    val nopes = mutable.HashSet[(Int, Int)]()
    var highest = -1
    var instructionPointer = 0
    var i = 0
    val stoneCount = offsetIterationCount * input.length + loopIterationsCount * input.length + remainingStones
    for (_ <- 0 until stoneCount) {
      val t = step(i, highest, instructionPointer, nopes)
      highest = t._2
      instructionPointer = t._1
      i += 1
    }
    highest + 1

  override def taskOneLogic(): String =
    val (heights, offsetIterationsCount, loopIterationsCount) = getLoopInformation()
    val offsetStoneCount = offsetIterationsCount * input.length
    val offsetHeight = heights.take(offsetIterationsCount).sum
    val loopStoneCount = loopIterationsCount * input.length
    val loopHeight = heights.iterator.slice(offsetIterationsCount, offsetIterationsCount + loopIterationsCount).sum
    val stonesCountWithoutOffset = 1000000000000L - offsetStoneCount.toLong
    val totalFullLoops = stonesCountWithoutOffset / loopStoneCount.toLong
    val remainingStoneCount = stonesCountWithoutOffset % loopStoneCount.toLong

    val remainingInfo = getRemainingHeightDiff(offsetIterationsCount, loopIterationsCount, remainingStoneCount.toInt)
    val remainingHeight = remainingInfo - offsetHeight - loopHeight

    val totalHeight = offsetHeight.toLong + totalFullLoops * loopHeight.toLong + remainingHeight.toLong
    totalHeight.toString

  private trait Shape {
    val offsets: Array[(Int, Int)]
    val highestOffset: Int

    def getPositions(position: (Int, Int)): Iterator[(Int, Int)] = offsets
      .iterator
      .map(o => (position._1 + o._1, position._2 + o._2))
    def collides(position: (Int, Int), nopes: mutable.HashSet[(Int, Int)]): Boolean =
      getPositions(position)
        .count(p => p._2 < 0 || p._1 < 0 || p._1 > 6 || nopes.contains(p)) > 0
  }

  private class Minus extends Shape {
    override val offsets: Array[(Int, Int)] = Array((0, 0), (1, 0), (2, 0), (3, 0))
    override val highestOffset: Int = offsets.iterator.map(_._2).max
  }

  private class Plus extends Shape {
    override val offsets: Array[(Int, Int)] = Array((1, 2), (0, 1), (1, 1), (2, 1), (1, 0))
    override val highestOffset: Int = offsets.iterator.map(_._2).max
  }

  private class MirroredL extends Shape {
    override val offsets: Array[(Int, Int)] = Array((2, 2), (2, 1), (0, 0), (1, 0), (2, 0))
    override val highestOffset: Int = offsets.iterator.map(_._2).max
  }

  private class I extends Shape {
    override val offsets: Array[(Int, Int)] = Array((0, 3), (0, 2), (0, 1), (0, 0))
    override val highestOffset: Int = offsets.iterator.map(_._2).max
  }

  private class Block extends Shape {
    override val offsets: Array[(Int, Int)] = Array((0, 1), (1, 1), (0, 0), (1, 0))
    override val highestOffset: Int = offsets.iterator.map(_._2).max
  }
}
