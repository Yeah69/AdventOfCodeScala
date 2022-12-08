import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

class Day07 extends Day {

  override val label: String = "07"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  private val cdRoot: Regex = "\\$ cd /".r
  private val cdUp: Regex = "\\$ cd \\.\\.".r
  private val cdDir: Regex = "\\$ cd (.*)".r
  private val ls: Regex = "\\$ ls".r
  private val infoDir: Regex = "dir (.+)".r
  private val infoFile: Regex = "([0-9]+) (.+)".r

  private val root: Node =
    val ret = Node()
    var current = ret
    for (elem <- input.linesIterator) {
      elem match
        case cdRoot() =>
        case cdUp() => current = current.parent
        case cdDir(name) => current = current.directories.filter(d => d._2 == name).head._1
        case ls() =>
        case infoDir(name) =>
          val newDir = Node()
          newDir.parent = current
          current.directories.addOne((newDir, name))
        case infoFile(sizeText, name) => current.files.addOne((sizeText.toInt, name))
    }
    ret.calculateSize()
    ret

  override def taskZeroLogic(): String = root.collectForTaskZero().toString

  override def taskOneLogic(): String = 
    val collection = ArrayBuffer[Int]()
    root.findForTaskOne(root.size - 40000000, collection)
    collection.min.toString

  class Node {
    var parent: Node = null
    val files: ArrayBuffer[(Int, String)] = ArrayBuffer()
    val directories: ArrayBuffer[(Node, String)] = ArrayBuffer()
    var size: Int = 0

    def calculateSize(): Unit =
      for (elem <- directories) { 
        elem._1.calculateSize() 
      }
      size = files.map((s, _) => s).sum + directories.map((d, _) => d.size).sum

    def collectForTaskZero(): Int =
      directories.map((d, _) => d.collectForTaskZero()).sum + (if size <= 100000 then size else 0)

    def findForTaskOne(neededSpace: Int, collection: ArrayBuffer[Int]): Unit =
      if size >= neededSpace then collection.addOne(size)
      for (elem <- directories) {
        elem._1.findForTaskOne(neededSpace, collection)
      }
  }
}
