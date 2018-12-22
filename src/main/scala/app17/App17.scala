package app17

import java.io.{File, PrintWriter}

import scala.io.Source

object App17 {

  val Spring = Pos(500, 0)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input17.txt").getLines()
    val regex = """(.)=(\d+), (.)=(\d+)\.\.(\d+)""".r

    val clayVeins = lines.map {
      case regex("x", value, "y", rangeStart, rangeEnd) => Clay.apply(value.toInt, rangeStart.toInt to rangeEnd.toInt)
      case regex("y", value, "x", rangeStart, rangeEnd) => Clay.apply(rangeStart.toInt to rangeEnd.toInt, value.toInt)
    }

    val clayMap = clayVeins.flatten.map(c => c.pos -> c).toMap

    val state = State(clayMap, Map(Spring -> FlowingWater), 0)

    val finalState = state.flow()

    val waterReached = finalState.stateWater.filterKeys(_ != Spring).filterKeys(p => p.y <= finalState.maxY && p.y >= finalState.minY).size
    println(s"Water reached $waterReached tiles")

    val waterRemaining = finalState.stateWater.filter {
      case (_, StableWater) => true
      case _ => false
    }

    println(s"Water remaining ${waterRemaining.size}")

    val pw = new PrintWriter(new File("/tmp/water.txt" ))
    pw.write(finalState.toString)
    pw.close
  }
}
