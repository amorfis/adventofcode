import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map}

object App9bCustomCollection {

//  val PlayersCount = 459
//  val MaxMarble = 71320
  //375414

  val PlayersCount = 459
  val MaxMarble = 7132000

//  val PlayersCount = 21
//  val MaxMarble = 6111
  //54718

//  val PlayersCount = 9
//  val MaxMarble = 25
  //32

  type Marble = Int
  type Player = Int
  type ScoreBoard = Map[Player, Long]

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
    def apply(): State = State(Map().withDefaultValue(0), Circle(new FastSeq(100000, 0), 0), 0, 0)
  }

  case class Circle(marbles: FastSeq[Marble], var currentMarble: Int) {

    def putMarble(marble: Marble) = {
//      println(s"Putting marble $marble")
//      println(this.toString)

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

    override def toString: String = marbles.toSeq.take(currentMarble).mkString(" ") + " [" + marbles(currentMarble) + "] " + marbles.toSeq.drop(currentMarble + 1).mkString(" ")
  }

  class FastSeq[E](maxBlockSize: Int, elems: E*) {

    private val blocks = mutable.Buffer[ArrayBuffer[E]](ArrayBuffer[E](elems: _*))

    var current = 0
    var currentBlockStartIdx = 0

    def apply(idx: Int) = {
      val blockIdx = whichBlockContains(idx)
      val block = blocks(blockIdx)

      block(idx - blockStartIdx(blockIdx))
    }

    def append(e: E): Unit = {
      current = blocks.size - 1
      currentBlockStartIdx = blockStartIdx(current)

      if (currentBlock.size >= maxBlockSize) {
        moveToNextBlock()
      }

      currentBlock.append(e)
    }

    private def moveToNextBlock(): Unit = {
      val currentBlockSize = currentBlock.size

      if (current == blocks.size - 1) {
        blocks.append(ArrayBuffer[E]())
      }

      current = current + 1
      currentBlockStartIdx = currentBlockStartIdx + currentBlockSize
    }

    def size = blocks.map(_.size).sum

    private def splitBlock(blockContaining: Player) = {
      val block = blocks(blockContaining)
      val (left, right) = block.splitAt(block.size / 2)
      blocks.update(blockContaining, left)
      blocks.insert(blockContaining + 1, right)
    }

    def insert(index: Int, elem: E): Unit = {
      if (index == size) {
        append(elem)
      } else {
        val blockContaining = whichBlockContains(index)
        if (blocks(blockContaining).size == maxBlockSize) {
          splitBlock(blockContaining)
          insert(index, elem)
        } else {
          blocks(blockContaining).insert(index - blockStartIdx(blockContaining), elem)
        }
      }
    }

    def remove(idx: Int) = {
      current = whichBlockContains(idx)
      currentBlockStartIdx = blockStartIdx(current)

      currentBlock.remove(idx - currentBlockStartIdx)
    }

    def whichBlockContains(idx: Int) = {
      idx match {
        case i if blockContainsIdx(current, i) => current
        case i if i < currentBlockStartIdx => searchForIdxFrom(0, i)
        case i => searchForIdxFrom(current, i)
      }
    }

    private def blockContainsIdx(searchedBlock: Int, idx: Int) = {
      if (searchedBlock > blocks.size - 1) {
        false
      } else {
        val blockStart = blockStartIdx(searchedBlock)
        blockStart <= idx && blockStart + blocks(searchedBlock).size > idx
      }
    }

    private def blockStartIdx(block: Int) = {
      blocks.take(block).foldLeft(0) { (curBlockStartIdx, curBlock) =>
        curBlockStartIdx + curBlock.size
      }
    }

    private def searchForIdxFrom(searchedBlock: Int, idx: Int): Int = {
      if (blockContainsIdx(searchedBlock, idx)) {
        searchedBlock
      } else {
        val nextBlockToSearch = searchedBlock + 1
        if (nextBlockToSearch == blocks.size) {
          nextBlockToSearch
        } else {
          searchForIdxFrom(searchedBlock + 1, idx)
        }
      }
    }

    private def currentBlock = blocks(current)

    private def currentBlockStart = current * maxBlockSize + currentBlock.size

    def toSeq = blocks.foldLeft(Seq[E]()) { (acc, block) =>
      acc ++ block
    }
  }

}


