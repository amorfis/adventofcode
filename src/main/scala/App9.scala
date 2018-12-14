object App9 {

  val PlayersCount = 459
  val MaxMarble = 71320

  type Marble = Int
  type Player = Int
  type ScoreBoard = Map[Player, Int]

  def main(args: Array[String]): Unit = {
    val lastState = nthState(MaxMarble, State())

    println(lastState.scoreBoard)

    println(lastState.scoreBoard.maxBy {
      case (p, s) => s
    })
  }

  def nthState(n: Int, initialState: State): State = {
    n match {
      case 0 => initialState
      case _ => nthState(n - 1, initialState.nextState)
    }
  }

  case class State(scoreBoard: ScoreBoard, circle: Circle, lastPlayer: Player, lastMarblePlayed: Marble) {
    def nextState = {
      val currentPlayer = (lastPlayer + 1) % PlayersCount
      val newCircle = circle.putMarble(lastMarblePlayed + 1)
      val newScoreBoard = scoreBoard + (currentPlayer -> (scoreBoard(currentPlayer) + newCircle.lastOpPoints))
      State(newScoreBoard, newCircle, currentPlayer, lastMarblePlayed + 1)
    }
  }

  object State {
    def apply(): State = State(Map().withDefaultValue(0), Circle(Seq(0), 0, 0), 0, 0)
  }

  case class Circle(marbles: Seq[Marble], currentMarble: Int, lastOpPoints: Int) {

    def putMarble(marble: Marble) = {
      println(s"Putting marble $marble")

      marble match {
        case 1 => Circle(Seq(0, 1), 1, 0)
        case m if m % 23 == 0 =>
          val removedMarbleIdx = fromCurrentClockwise(-7)
          val removedMarble = marbles(removedMarbleIdx)
          // remove marble
          val newMarbles = marbles.take(removedMarbleIdx) ++ marbles.drop(removedMarbleIdx + 1)
          Circle(newMarbles, removedMarbleIdx, marble + removedMarble)
        case m =>
          val newMarbleIdx = fromCurrentClockwise(2)
          val newMarbles = (marbles.take(newMarbleIdx) :+ marble) ++ marbles.drop(newMarbleIdx)
          Circle(newMarbles, newMarbleIdx, 0)
      }
    }

    def fromCurrentClockwise(step: Int) = {
      val newCurrent = (currentMarble + step) % marbles.size
      val newValue = if (newCurrent >= 0) {
        newCurrent
      }
      else {
        marbles.size + newCurrent
      }

      if (newValue == 0) {
        marbles.size
      } else {
        newValue
      }
    }

    override def toString: String = marbles.take(currentMarble).mkString(" ") + " [" + marbles(currentMarble) + "] " + marbles.drop(currentMarble + 1).mkString(" ")
  }

}


