package app18b

case class Loop(start: Int, size: Int)

case class MemoizedStateStream(statesMemo: Seq[State], loop: Option[Loop], last: Int) {

  def getFromLoop(i: Int) = {
    loop match {
      case Some(Loop(loopStart, loopSize)) =>
        val inLoopOne = ((i - loopStart) % loopSize) + loopStart
        statesMemo(inLoopOne)
    }
  }

  def next(): MemoizedStateStream = {
    loop match {
      case Some(_) =>
        MemoizedStateStream(statesMemo, loop, last + 1)

      case None =>
        val nextState = statesMemo.last.next()

        val indexOfCopy = statesMemo.map(_.forest).indexOf(nextState.forest)
        val newLoop = if (indexOfCopy > -1) {
          Some(Loop(indexOfCopy, statesMemo.size - indexOfCopy))
        } else {
          None
        }

        MemoizedStateStream(statesMemo :+ nextState, newLoop, last + 1)
    }
  }

  def getLast() = {
    loop match {
      case Some(Loop(loopStart, loopSize)) =>
        val nextOne = statesMemo.size
        val inLoopNextOne = ((nextOne - loopStart) % loopSize) + loopStart

        statesMemo(inLoopNextOne)

      case None =>
        statesMemo.last
    }
  }

}
