import System.nanoTime
// Copied from https://stackoverflow.com/a/14574121/4871837
def profile[R](code: => R, t: Long = nanoTime) = (code, nanoTime - t)

trait Day {
  val label: String
  val input: String
  def taskZeroLogic(): String
  def taskOneLogic(): String

  private def execute(taskLogic: () => String, taskLabel: String): Unit = {
    val (answer, timeInMillis) = profile { taskLogic() }
    println(s"Answer $taskLabel =")
    println(answer)
    println(s"(The task took $timeInMillis ns)")
    println()
  }

  private def taskZeroExecution(): Unit = {
    execute(taskZeroLogic, "Zero")
  }
  private def taskOneExecution(): Unit = {
    execute(taskOneLogic, "One")
  }

  def executeWholeDay(): Unit = {
    println()
    println(s"Day $label")
    println()
    println("Input =")
    println(input)
    println()
    val (_, timeInMillis) = profile {
      taskZeroExecution()
      taskOneExecution()
    }
    println(s"(The day $label took $timeInMillis ns in total)")
  }
}
