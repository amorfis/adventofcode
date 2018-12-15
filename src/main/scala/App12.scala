import scala.io.Source

object App12 {

  val InitialState = ".#..##..#.....######.....#....####.##.#.#...#...##.#...###..####.##.##.####..######......#..##.##.##"

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input12.txt").getLines()
    val r = """(.....) => (.)""".r
    val rules = lines.map {
      case r(cause, result) => Rule(cause, result)
    }

    val state = State(InitialState, rules.toSeq, 0)

    val lastState = rollState(state, 20)

    val indices = lastState.state.zipWithIndex.map {
      case(p, idx) =>
        val realIdx = idx + lastState.firstPotIdx
        if (p == '.') {
          0
        } else {
          realIdx
        }
    }

    println(indices)
    println(indices.sum)
  }

  def rollState(state: State, times: Int): State = {
    println(state.state)

    if (times == 0) {
      state
    } else {
      rollState(state.nextState, times - 1)
    }
  }

  case class State(state: String, rules: Seq[Rule], firstPotIdx: Int) {

    def nextState = {
      val stateWithTrailing = "...." + state
      val seqs: Seq[((((Char, Char), Char), Char), Char)] = stateWithTrailing
        .zipAll(stateWithTrailing.drop(1), '.', '.')
        .zipAll(stateWithTrailing.drop(2), ('.', '.'), '.')
        .zipAll(stateWithTrailing.drop(3), (('.', '.'), '.'), '.')
        .zipAll(stateWithTrailing.drop(4), ((('.', '.'), '.'), '.'), '.')

      val newState = seqs.map {
        case ((((c1, c2), c3), c4), c5) =>
          val pots = c1.toString + c2.toString + c3.toString + c4.toString + c5.toString
          rules.find(_.matches(pots)).map { rule =>
            rule.result
          }
          .getOrElse(pots.substring(2, 3))
      }
        .mkString("")

      State(newState, rules, firstPotIdx - 2)
    }
  }

  case class Rule(cause: String, result: String) {
    {
      if (cause.length != 5 || result.length != 1) {
        throw new RuntimeException(s"Wrong rule: $cause => $result")
      }
    }

    def matches(pots: String) = {
      pots == cause
    }
  }
}


