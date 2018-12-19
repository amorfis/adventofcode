import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object App14b {

  def main(args: Array[String]): Unit = {

//    val lookFor = Seq(9, 2, 5, 1, 0)
    val lookFor = Seq(6, 8, 1, 9, 0, 1)

    val finalState = rollUntilDigitsAppear(State(), lookFor)

    println(finalState.recipes.size - lookFor.size)
  }

  def rollUntilDigitsAppear(state: State, digits: Seq[Int]): State = {
    if (state.recipes.size % 10000 == 0) {
      println(s"Current recipes count: ${state.recipes.size}")
    }


    for {
      i <- 1 to 20000000
    } {
      if (i % 10000 == 0) {
        println(s"Rotation $i")
      }
      state.next()
    }

    val i = state.recipes.indexOfSlice(Seq(6, 8, 1, 9, 0, 1))

    println(i)

    state
//    if (state.recipes.matchLastOrOneFromLast(digits)) {
//      state
//    } else {
//      state.next()
//      rollUntilDigitsAppear(state, digits)
//    }
  }

  case class State(recipes: ArrayBuffer[Int], var elfOneCurrent: Int, var elfTwoCurrent: Int) {
    def generateNewRecipes() = {
//      val start = System.nanoTime()
      recipes.appendAll(toSeqOfDigits(recipes(elfOneCurrent) + recipes(elfTwoCurrent)))
//      val end = System.nanoTime()
//      if (recipes.size % 10000 == 0) {
//        println("Adding new recipes: " + (end - start))
//      }
    }

    def next() = {
      generateNewRecipes()
      elfOneCurrent = (elfOneCurrent + 1 + recipes(elfOneCurrent)) % recipes.size
      elfTwoCurrent = (elfTwoCurrent + 1 + recipes(elfTwoCurrent)) % recipes.size
    }

    override def toString: String = {
      def inParen(idx: Int) = {
        if (idx == elfOneCurrent) {
          "(" + recipes(idx) + ")"
        } else {
          "[" + recipes(idx) + "]"
        }
      }

      val minElf = Seq(elfOneCurrent, elfTwoCurrent).min
      val maxElf = Seq(elfOneCurrent, elfTwoCurrent).max

      val rSeq = recipes.toSeq
      rSeq.take(minElf).mkString(" ") +
        inParen(minElf) +
        rSeq.slice(minElf + 1, maxElf).mkString(" ") +
        inParen(maxElf) +
        rSeq.drop(maxElf + 1).mkString(" ")
    }
  }

  def toSeqOfDigits(number: Int) = {
    number.toString.toCharArray.map(_.toString.toInt)
  }

  object State {

    def apply() = {
      val s = ArrayBuffer(3, 7)
      new State(s, 0, 1)
    }
  }

  class FastSeq[E](maxBlockSize: Int, elems: E*) {

    class Block(var startIdx: Int, val inBlock: ArrayBuffer[E])

    private val blocks = mutable.Buffer[Block](new Block(0, ArrayBuffer[E](elems: _*)))

    var current = 0
    var currentBlockStartIdx = 0

    def matchLastOrOneFromLast(pattern: Seq[E]) = {
      if (size > pattern.size) {
        val lastBlock = blocks.last

        val maybePattern = if (lastBlock.inBlock.size > pattern.size) {
          lastBlock.inBlock.drop(lastBlock.inBlock.size - (pattern.size + 1))
        } else {
          val blockBeforeLast = blocks.reverse.tail.head
          blockBeforeLast.inBlock.drop(blockBeforeLast.inBlock.size - (pattern.size - lastBlock.inBlock.size + 1)) ++ lastBlock.inBlock
        }

        maybePattern.drop(maybePattern.size - pattern.size) == pattern
      } else if (size == pattern.size) {
        if (blocks.head.inBlock.size != pattern.size) {
          throw new RuntimeException("Dupa!")
        } else {
          blocks.head == pattern
        }
      } else {
        false
      }
    }

    def apply(idx: Int) = {
      val start = System.nanoTime()

      val blockIdx = whichBlockContains(idx)

      val end = System.nanoTime()
      if (idx % 10000 == 0) {
        println(s"Looking for the right block took: ${end - start} for $idx")
      }

      val block = blocks(blockIdx)

      block.inBlock(idx - blockStartIdx(blockIdx))
    }

    def append(e: E): Unit = {
      current = blocks.size - 1

//      val logIndex = if (size % 10000 == 0) {
//        true
//      } else {
//        false
//      }

      currentBlockStartIdx = blockStartIdx(current)
//      currentBlockStartIdx = blockStartIdx(current, logIndex)

      if (currentBlock.inBlock.size >= maxBlockSize) {
        moveToNextBlock()
      }

      currentBlock.inBlock.append(e)
    }

    def appendAll(elems: Seq[E]) = {
      elems.foreach(append)
      this
    }

    private def moveToNextBlock(): Unit = {
      val currentBlockSize = currentBlock.inBlock.size

      if (current == blocks.size - 1) {
        blocks.append(new Block(size, ArrayBuffer[E]()))
      }

      current = current + 1
      currentBlockStartIdx = currentBlockStartIdx + currentBlockSize
    }

    def size = blocks.last.startIdx + blocks.last.inBlock.size

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
        blockStart <= idx && blockStart + blocks(searchedBlock).inBlock.size > idx
      }
    }

    private def blockStartIdx(block: Int, log: Boolean = false) = {
      val start = System.nanoTime()
      val r = blocks.take(block).foldLeft(0) { (curBlockStartIdx, curBlock) =>
        curBlockStartIdx + curBlock.inBlock.size
      }

      val end = System.nanoTime()

      if (log) {
        println(s"Counting block start idx: ${end - start}")
      }

      r
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

    def toSeq = blocks.foldLeft(Seq[E]()) { (acc, block) =>
      acc ++ block.inBlock
    }
  }
}


