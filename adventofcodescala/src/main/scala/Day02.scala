import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.*

class Day02 extends Day {

  override val label: String = "02"
  override val input: String = Source.fromResource(s"Day$label.txt").mkString

  override def taskZeroLogic(): String =
    input.linesIterator
      .map({ line =>
        val opponent = line(0) match
          case 'A' => "Rock"
          case 'B' => "Paper"
          case 'C' => "Scissor"
          case _ => noSolutionFound
        val me = line(2) match
          case 'X' => "Rock"
          case 'Y' => "Paper"
          case 'Z' => "Scissor"
          case _ => noSolutionFound
        (opponent, me)
      })
      .map({ tuple =>
        val choicePoints = tuple._2 match
          case "Rock" => 1
          case "Paper" => 2
          case "Scissor" => 3
          case _ => 0
        val resultPoints = if tuple._1 == tuple._2 then 3
        else if tuple._1 == "Rock" && tuple._2 == "Paper"
          || tuple._1 == "Paper" && tuple._2 == "Scissor"
          || tuple._1 == "Scissor" && tuple._2 == "Rock" then 6
        else 0
        choicePoints + resultPoints
      })
      .sum
      .toString

  override def taskOneLogic(): String =
    input.linesIterator
      .map({ line =>
        val opponent = line(0) match
          case 'A' => "Rock"
          case 'B' => "Paper"
          case 'C' => "Scissor"
          case _ => noSolutionFound
        val me = line(2) match
          case 'X' => "Loose"
          case 'Y' => "Draw"
          case 'Z' => "Win"
          case _ => noSolutionFound
        (opponent, me)
      })
      .map({ tuple =>
        val choicePoints = tuple match
          case ("Rock", "Loose") => 3
          case ("Rock", "Draw") => 1
          case ("Rock", "Win") => 2
          case ("Paper", "Loose") => 1
          case ("Paper", "Draw") => 2
          case ("Paper", "Win") => 3
          case ("Scissor", "Loose") => 2
          case ("Scissor", "Draw") => 3
          case ("Scissor", "Win") => 1
          case _ => 0
        val resultPoints = tuple._2 match
          case "Loose" => 0
          case "Draw" => 3
          case "Win" => 6
          case _ => -1
        choicePoints + resultPoints
      })
      .sum
      .toString
}
