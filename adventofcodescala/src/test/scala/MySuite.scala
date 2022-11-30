// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("day 01") {
    val taskZero = "70698"
    val taskOne = "206643"
    val day = Day01()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 02") {
    val taskZero = "12679"
    val taskOne = "14470"
    val day = Day02()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 03") {
    val taskZero = "7967"
    val taskOne = "2716"
    val day = Day03()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 04") {
    val taskZero = "433"
    val taskOne = "852"
    val day = Day04()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 05") {
    val taskZero = "HBTMTBSDC"
    val taskOne = "PQTJRSHWS"
    val day = Day05()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 06") {
    val taskZero = "1920"
    val taskOne = "2334"
    val day = Day06()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 07") {
    val taskZero = "1182909"
    val taskOne = "2832508"
    val day = Day07()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 08") {
    val taskZero = "1787"
    val taskOne = "440640"
    val day = Day08()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 09") {
    val taskZero = "6030"
    val taskOne = "2545"
    val day = Day09()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 10") {
    val taskZero = "14340"
    val taskOne = "PAPJCBHP"
    val day = Day10()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 11") {
    val taskZero = "58794"
    val taskOne = "20151213744"
    val day = Day11()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 12") {
    val taskZero = "339"
    val taskOne = "332"
    val day = Day12()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 13") {
    val taskZero = "6072"
    val taskOne = "22184"
    val day = Day13()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 14") {
    val taskZero = "719"
    val taskOne = "23390"
    val day = Day14()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 15") {
    val taskZero = "5525990"
    val taskOne = "11756174628223"
    val day = Day15()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 16") {
    val taskZero = "1828"
    val taskOne = "2292"
    val day = Day16()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 17") {
    val taskZero = "3175"
    val taskOne = "1555113636385"
    val day = Day17()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 18") {
    val taskZero = "3454"
    val taskOne = "2014"
    val day = Day18()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 19") {
    val taskZero = "1962"
    val taskOne = "88160"
    val day = Day19()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 20") {
    val taskZero = "19070"
    val taskOne = "14773357352059"
    val day = Day20()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 21") {
    val taskZero = "118565889858886"
    val taskOne = "3032671800353"
    val day = Day21()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 22") {
    val taskZero = "95358"
    val taskOne = "144361"
    val day = Day22()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 23") {
    val taskZero = "4181"
    val taskOne = "973"
    val day = Day23()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 24") {
    val taskZero = "247"
    val taskOne = "728"
    val day = Day24()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
  test("day 25") {
    val taskZero = "2-0-0=1-0=2====20=-2"
    val taskOne = nothingToDoHere
    val day = Day25()
    assertEquals(day.taskZeroLogic(), taskZero)
    assertEquals(day.taskOneLogic(), taskOne)
  }
}
