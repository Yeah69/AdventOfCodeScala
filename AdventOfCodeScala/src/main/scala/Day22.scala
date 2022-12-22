import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day22 extends Day {
  private enum Direction:
    case Up, Down, Left, Right

  private enum Tile:
    case Floor, Wall

  override val label: String = "22"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val data: (Set[(Int, Int)], Set[(Int, Int)], Array[Int|Direction]) =
    val parts = input.split(sys.props("line.separator") + sys.props("line.separator"))
    val map = parts(0)
    val instructionText = parts(1)

    val tiles = map.linesIterator
      .zipWithIndex
      .flatMap((row, y) =>
        row.zipWithIndex.filter((c, _) => c == '.' || c == '#').map((c, x) => (if c == '.' then Tile.Floor else Tile.Wall, x + 1, y + 1)))
      .toArray

    val floor = tiles.filter((t, _, _) => t == Tile.Floor).map((_, x, y) => (x, y)).toSet
    val wall = tiles.filter((t, _, _) => t == Tile.Wall).map((_, x, y) => (x, y)).toSet

    val instructionArray: ArrayBuffer[Int|Direction] = ArrayBuffer()
    var i = 0
    while(i < instructionText.length) {
      if instructionText(i) == 'L' then
        i += 1
        instructionArray.addOne(Direction.Left)
      else if instructionText(i) == 'R' then
        i += 1
        instructionArray.addOne(Direction.Right)
      else
        val start = i
        while (i < instructionText.length && instructionText(i).isDigit) {
          i += 1
        }
        instructionArray.addOne(instructionText.substring(start, i).toInt)
    }

    (floor, wall, instructionArray.toArray)

  private def calculatePassword(position: (Int, Int), facing: Direction): Int =
    val facingValue = facing match
      case Direction.Right => 0
      case Direction.Down => 1
      case Direction.Left => 2
      case Direction.Up => 3
    1000 * position._2 + 4 * position._1 + facingValue

  private def addPosition(position: (Int, Int), toAdd: (Int, Int)): (Int, Int) =
    (position._1 + toAdd._1, position._2 + toAdd._2)

  private def getNextPotentialPosition(position: (Int, Int), facing: Direction): (Int, Int) =
    val toAdd = facing match
      case Direction.Up => (0, -1)
      case Direction.Down => (0, 1)
      case Direction.Left => (-1, 0)
      case Direction.Right => (1, 0)
    addPosition(position, toAdd)

  private def getNextPositionAndFacing(position: (Int, Int), nextPotentialPosition: (Int, Int), facing: Direction,
                                       wrapMod: ((Int, Int), Direction) => ((Int, Int), Direction)): ((Int, Int), Direction) =
    val (floor, wall, _) = data
    if floor.contains(nextPotentialPosition) then
      (nextPotentialPosition, facing)
    else if wall.contains(nextPotentialPosition) then
      (position, facing)
    else
      val (tempPotentialPosition, tempPotentialDirection) = wrapMod(position, facing)
      if wall.contains(tempPotentialPosition) then
        return (position, facing)
      getNextPositionAndFacing(position, tempPotentialPosition, tempPotentialDirection, wrapMod)

  private def taskLogic(wrapMod: ((Int, Int), Direction) => ((Int, Int), Direction)): String =
    val (floor, _, instructions) = data
    var position = (floor.filter((_, y) => y == 1).map(_._1).min, 1)
    var facing = Direction.Right

    for (instruction <- instructions) {
      instruction match
        case i: Int =>
          for (_ <- 0 until i) {
            val tuple = getNextPositionAndFacing(position, getNextPotentialPosition(position, facing), facing, wrapMod)
            position = tuple._1
            facing = tuple._2
          }
        case d: Direction =>
          facing = (facing, d) match
            case (Direction.Up, Direction.Left) => Direction.Left
            case (Direction.Down, Direction.Left) => Direction.Right
            case (Direction.Left, Direction.Left) => Direction.Down
            case (Direction.Right, Direction.Left) => Direction.Up
            case (Direction.Up, Direction.Right) => Direction.Right
            case (Direction.Down, Direction.Right) => Direction.Left
            case (Direction.Left, Direction.Right) => Direction.Up
            case (Direction.Right, Direction.Right) => Direction.Down
            case _ => facing // shouldn't happen as input has only Left and Right
    }

    calculatePassword(position, facing).toString

  override def taskZeroLogic(): String =
    val (floor, wall, _) = data
    taskLogic((position, facing) =>
      val toAdd = facing match
        case Direction.Up => (0, 1)
        case Direction.Down => (0, -1)
        case Direction.Left => (1, 0)
        case Direction.Right => (-1, 0)
      var tempPotentialPosition = position
      var checkedPosition = addPosition(tempPotentialPosition, toAdd)
      while (floor.contains(checkedPosition) || wall.contains(checkedPosition)) {
        tempPotentialPosition = checkedPosition
        checkedPosition = addPosition(tempPotentialPosition, toAdd)
      }
      (tempPotentialPosition, facing))

  override def taskOneLogic(): String =
    val map = (51 to 100).zip(151 to 200).map((i, j) => (((i, 1), Direction.Up), ((1, j), Direction.Right)))
      .concat((151 to 200).zip(51 to 100).map((i, j) => (((1, i), Direction.Left), ((j, 1), Direction.Down))))

      .concat((101 to 150).zip(1 to 50).map((i, j) => (((i, 1), Direction.Up), ((j, 200), Direction.Up))))
      .concat((1 to 50).zip(101 to 150).map((i, j) => (((i, 200), Direction.Down), ((j, 1), Direction.Down))))

      .concat((1 to 50).zip(150 to 101 by -1).map((i, j) => (((51, i), Direction.Left), ((1, j), Direction.Right))))
      .concat((150 to 101 by -1).zip(1 to 50).map((i, j) => (((1, i), Direction.Left), ((51, j), Direction.Right))))

      .concat((1 to 50).zip(150 to 101 by -1).map((i, j) => (((150, i), Direction.Right), ((100, j), Direction.Left))))
      .concat((150 to 101 by -1).zip(1 to 50).map((i, j) => (((100, i), Direction.Right), ((150, j), Direction.Left))))

      .concat((51 to 100).zip(1 to 50).map((i, j) => (((51, i), Direction.Left), ((j, 101), Direction.Down))))
      .concat((1 to 50).zip(51 to 100).map((i, j) => (((i, 101), Direction.Up), ((51, j), Direction.Right))))

      .concat((51 to 100).zip(101 to 150).map((i, j) => (((100, i), Direction.Right), ((j, 50), Direction.Up))))
      .concat((101 to 150).zip(51 to 100).map((i, j) => (((i, 50), Direction.Down), ((100, j), Direction.Left))))

      .concat((51 to 100).zip(151 to 200).map((i, j) => (((i, 150), Direction.Down), ((50, j), Direction.Left))))
      .concat((151 to 200).zip(51 to 100).map((i, j) => (((50, i), Direction.Right), ((j, 150), Direction.Up))))

      .toMap
    taskLogic((position, facing) => map((position, facing)))
}
