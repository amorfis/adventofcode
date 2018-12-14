import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map}
import scala.concurrent.duration.Duration

object App9bMutable {

  val PlayersCount = 459
  val MaxMarble = 71320000

  type Marble = Int
  type Player = Int
  type ScoreBoard = Map[Player, Int]

  def main(args: Array[String]): Unit = {
    val state = State()
    for {
      _ <- 0 to MaxMarble
    } {
      state.nextState
    }
    println(state.scoreBoard)

    println(state.scoreBoard.maxBy {
      case (p, s) => s
    })
  }

  var lastTime = System.currentTimeMillis()

  case class State(scoreBoard: ScoreBoard, circle: Circle, var lastPlayer: Player, var lastMarblePlayed: Marble) {
    def nextState = {
      if (lastMarblePlayed % 1000 == 0) {
        val duration = Duration(System.currentTimeMillis() - lastTime, TimeUnit.MILLISECONDS)
        println(s"Last marble: $lastMarblePlayed")
        println(s"Current marble idx: ${circle.currentMarble}")
        println(s"Last 1000 took: $duration")

        lastTime = System.currentTimeMillis()
      }

      val currentPlayer = (lastPlayer + 1) % PlayersCount
      val points = circle.putMarble(lastMarblePlayed + 1)

      if (points > 0) {
        scoreBoard.put(currentPlayer, scoreBoard(currentPlayer) + points)
      }

      lastPlayer = currentPlayer
      lastMarblePlayed = lastMarblePlayed + 1
    }
  }

  object State {
    def apply(): State = State(Map().withDefaultValue(0), Circle(mutable.ArrayBuffer(0), 0), 0, 0)
  }

  case class Circle(marbles: mutable.ArrayBuffer[Marble], var currentMarble: Int) {

    def putMarble(marble: Marble) = {
//      println(s"Putting marble $marble")

      marble match {
        case 1 => {
          marbles.append(1)
          currentMarble = 1
          0
        }
        case m if m % 23 == 0 =>
          val removedMarbleIdx = fromCurrentClockwise(-7)
          val removedMarble = marbles(removedMarbleIdx)
          // remove marble
          marbles.remove(removedMarbleIdx)
          currentMarble = removedMarbleIdx
          marble + removedMarble
        case m =>
          val newMarbleIdx = fromCurrentClockwise(2)
          marbles.insert(newMarbleIdx, marble)
          currentMarble = newMarbleIdx
          0
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


