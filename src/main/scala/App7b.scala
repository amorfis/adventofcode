import scala.io.Source

object App7b {

  case class Condition(before: Task, after: Task)

  type Task = Char

  val Workers = 5

  case class Step(taskPerWorker: Seq[Option[Task]])

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("input7.txt").getLines().toList
    val rg = """Step (.) must be finished before step (.) can begin.""".r

    val pairs = lines.map {
      case rg(first, second) => Condition(first.charAt(0), second.charAt(0))
    }

    val allSteps = pairs.toSet.flatMap( (p: Condition) => Set(p.before, p.after))
    val readyOnes = allSteps diff pairs.map(_.after).toSet

    val conditions = pairs.groupBy(_.after).mapValues(_.map(_.before))

    val conditionsWithEmptyOnes = conditions ++ readyOnes.map(step => (step, Seq[Task]())).toMap

    val steps = new StepsCalculator(conditionsWithEmptyOnes).nextSteps(Seq())

    println(steps.size - 1)
  }

  class StepsCalculator(conditions: Map[Task, Seq[Task]]) {

    def nextSteps(prevSteps: Seq[Step]): Seq[Step] = {
      if (prevSteps.nonEmpty && !prevSteps.last.taskPerWorker.exists(_.isDefined)) {
        prevSteps
      } else {
        nextSteps(prevSteps :+ nextStep(prevSteps))
      }
    }

    def nextStep(prevSteps: Seq[Step]) = {
      val finished = finishedTasks(prevSteps).toSeq
      val readyTasks = calculateReadyTasks(finished).toSeq diff finished

      val padded = readyTasks.map(Some(_)).padTo(Workers, None)

      val tasksForWorkers = for {
        w <- 0 until Workers
      } yield {
        padded(w)
      }

      val s = Step(tasksForWorkers)
      println(s)

      s
    }

    def calculateReadyTasks(finishedTasks: Seq[Task]) = {
      conditions.filter(tuple => (tuple._2 diff finishedTasks).isEmpty).keys
    }
  }

  def finishedTasks(finishedSteps: Seq[Step]) = {
    val secondsPerTask = finishedSteps.foldLeft(Map[Task, Long]().withDefaultValue(0L)) { (map, step) =>
      val tasksInStep = step.taskPerWorker.filter(_.isDefined).map(_.get)
      tasksInStep.foldLeft(map) {
        case (innerMap, task) =>
          val oldValue: Long = innerMap(task)
          val newValue = oldValue + 1L
          (innerMap + (task -> newValue)).withDefaultValue(0L)
      }
    }

    secondsPerTask.keys.map { task =>
      if (secondsPerTask(task) == timeForTask(task)) {
        Some(task)
      } else {
        None
      }
    }
      .collect {
        case Some(task) => task
      }
  }

  def timeForTask(task: Task) = task.toInt - 65 + 1 + 60
}


