import scala.collection.{immutable, mutable}
import scala.io.Source

object App1b {

  def findRepeatingState(state: Stream[Long]) = {
    val stateWithPrevious = streamWithAllPrevious(state, 1)

    val withRepetition = stateWithPrevious.find { states =>
      val withoutLast = states.reverse.tail
      val last = states.last

      withoutLast.contains(last)
    }

    withRepetition.get.last
  }

  def findRepeatingStateFast(states: Stream[Long]) = {
    val prevStates = mutable.TreeSet[Long]()
    val statesIterator = states.iterator

    var found: Option[Long] = None

    while(found.isEmpty) {
      val state = statesIterator.next()

      println(s"Fast one. Checked ${prevStates.size} states")

      if (prevStates.contains(state)) {
        found = Some(state)
      }

      prevStates.add(state)
    }

    found.get
  }

  def main(args: Array[String]): Unit = {
//    val lines = Source.fromResource("fakeinput.txt").getLines().map(_.toLong)
    val lines = Source.fromResource("input1.txt").getLines().map(_.toLong)
    val changesStream = stream(lines.toList, 0)
    val state = stateStream(0, changesStream)

    println(findRepeatingStateFast(state))
  }

  def streamWithAllPrevious(simple: Iterable[Long], howMany: Int): Stream[Seq[Long]] = {
    val seq = simple.take(howMany).toSeq
    println("Stream with previous: " + seq.size)
    seq #:: streamWithAllPrevious(simple, howMany + 1)
  }

  def stream(list: Seq[Long], index: Int): Stream[Long] = {
    if (index < list.size) {
      list(index) #:: stream(list, index + 1)
    } else {
      list.head #:: stream(list, 1)
    }
  }

  def stateStream(prevState: Long, changes: Iterable[Long]): Stream[Long] = {
    val state = prevState + changes.head
    state #:: stateStream(state, changes.tail)
  }

}
