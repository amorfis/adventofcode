import java.util.concurrent.TimeUnit

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration

object App9bFast {

//  val PlayersCount = 459
  val PlayersCount = 459
//  val MaxMarble = 71320
  val MaxMarble = 7132000

  val SliceSize = 100000

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

  var lastTime = System.currentTimeMillis()

  case class State(scoreBoard: ScoreBoard, circle: Circle, lastPlayer: Player, lastMarblePlayed: Marble) {
    def nextState = {

//      println(s"CurrentMarble: ${circle.currentMarble}")
      if (lastMarblePlayed % 1000 == 0) {
        val duration = Duration(System.currentTimeMillis() - lastTime, TimeUnit.MILLISECONDS)
        println(s"Last marble: $lastMarblePlayed")
        println(s"Last 1000 took: $duration")

        lastTime = System.currentTimeMillis()
      }

      val currentPlayer = (lastPlayer + 1) % PlayersCount
      val newCircle = circle.putMarble
      val newScoreBoard = scoreBoard + (currentPlayer -> (scoreBoard(currentPlayer) + newCircle.lastOpPoints))
      State(newScoreBoard, newCircle, currentPlayer, lastMarblePlayed + 1)
    }

//    def rollStates(lastMarble: Marble) = {
//
//    }
  }

  object State {
    def apply(): State = {
      val marbles = ArrayBuffer(0)
      marbles.sizeHint(8000000)
      State(Map().withDefaultValue(0), Circle(marbles, 0, 0, 1), 0, 0)
    }
  }

  case class Circle(marbles: ArrayBuffer[Marble], currentMarble: Int, lastOpPoints: Int, nextMarble: Marble) {

    def putMarble = {
      val marble = nextMarble
      marble match {
        case 1 => Circle(ArrayBuffer(0, 1), 1, 0, marble + 1)
        case m if m % 23 == 0 =>
          val removedMarbleIdx = fromCurrentClockwise(-7)
          val removedMarble = marbles(removedMarbleIdx)
          // remove marble
          val newMarbles = marbles.take(removedMarbleIdx) ++ marbles.drop(removedMarbleIdx + 1)
          Circle(newMarbles, removedMarbleIdx, marble + removedMarble, marble + 1)
        case m =>
          val newMarbleIdx = fromCurrentClockwise(2)
          val newMarbles = (marbles.take(newMarbleIdx) :+ marble) ++ marbles.drop(newMarbleIdx)
          Circle(newMarbles, newMarbleIdx, 0, marble + 1)
      }
    }

//    def putManyMarbles() = {
//      val marblesTo23 = 23 - nextMarble % 23
//      if (currentMarble + marblesTo23 > marbles.size) {
//        putMarble()
//      } else {
//        newMarbles =
//      }
//    }

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


