package app18b

import scala.io.Source

object App18b {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input18.txt").getLines()

    val forest = for {
      (line, li) <- lines.zipWithIndex
      (c, i) <- line.zipWithIndex
    } yield {
      val f = c match {
        case '.' => Space
        case '|' => Tree
        case '#' => Lumberyard
      }

      Pos(i, li) -> f
    }

    val state0 = State(forest.toMap, 0)



    val iterator = Iterator.iterate(MemoizedStateStream(Seq(state0), None, 0)) { m =>
      println(m.last)
      m.next()
    }

    val lastNoLoop = iterator.takeWhile(_.loop.isEmpty).toSeq.last

    val testState = lastNoLoop.getLast().next().next()
    val testRev = testState.rev
    println(s"Test rev: $testRev")

    val withLoop = iterator.next()

    val testStateFromLoop = withLoop.getFromLoop(testRev)
    println(s"Test state rev: ${testStateFromLoop.rev}")

    println(s"States equal: ${testState.forest == testStateFromLoop.forest}")


    val state = withLoop.getFromLoop(1000000000)


    val resValue = state.forest.values.count(_.isTree) * state.forest.values.count(_.isLumberyard)

    println(resValue)
  }
}
