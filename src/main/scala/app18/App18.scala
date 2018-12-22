package app18

import scala.io.Source

object App18 {

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

    val iterator = Iterator.iterate(state0) { s =>
      println(s)
      s.next()
    }

    val last = iterator.take(11).toStream.last

    val resValue = last.forest.values.count(_.isTree) * last.forest.values.count(_.isLumberyard)

    println(last)
    println(resValue)
  }
}
