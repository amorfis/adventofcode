import scala.io.Source

object App7 {

  case class Condition(before: String, after: String)

  def main(args: Array[String]): Unit = {

    val lines = Source.fromResource("input7.txt").getLines().toList
    val rg = """Step (.) must be finished before step (.) can begin.""".r

    val pairs = lines.map {
      case rg(first, second) => Condition(first, second)
    }

    val allSteps = pairs.toSet.flatMap( (p: Condition) => Set(p.before, p.after))
    val readyOnes = allSteps diff pairs.map(_.after).toSet

    val conditions = pairs.groupBy(_.after).mapValues(_.map(_.before))

    val conditionsWithEmptyOnes = conditions ++ readyOnes.map(step => (step, Seq[String]())).toMap

    val steps = nextSteps(conditionsWithEmptyOnes)

    println(steps)
  }

  def nextSteps(conditions: Map[String, Seq[String]]): String = {
    val nextStep = readySteps(conditions).toSeq.sorted.headOption

    nextStep match {
      case Some(step) =>
        val conditionsWithStepDone = conditions.filterKeys(_ != step).mapValues(_.filterNot(_ == step))
        step + nextSteps(conditionsWithStepDone)
      case None => ""
    }
  }

  def readySteps(conditions: Map[String, Seq[String]]) = conditions.filter(_._2.isEmpty).keys
}


