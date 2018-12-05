import java.text.SimpleDateFormat
import java.time._
import java.util.{Date, TimeZone}

import scala.io.Source
import scala.util.matching.Regex

object App4b {

  val lineRegex: Regex = """\[(.*)\] (.*)""".r

  type GuardId = Long
  type Minute = Int
  type MinutesAsleep = Seq[Minute]

  def main(args: Array[String]): Unit = {

    def moveTimeBackToFirstHourEnd(time: LocalDateTime) = {
      if (time.getHour > 0) {
        time.withHour(1).withMinute(0)
      } else {
        time
      }
    }

    val timeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    timeFormat.setTimeZone(TimeZone.getTimeZone("UTC"))

    val lines = Source.fromResource("input4.txt").getLines()
    val events = lines.map {
      case lineRegex(timestamp, message) =>
        val time = LocalDateTime.from(timeFormat.parse(timestamp).toInstant.atZone(ZoneId.of("UTC")))
        val event = EventType.parse(message)

        Event(time, event)
    }
      .toSeq.sortBy(_.time.toInstant(ZoneOffset.UTC))

    events.foreach(e => println(e.time + "    " + e.eventType.toString))

    val shifts = events.foldLeft(Seq[Shift]()) { (historyList, event) =>
      event.eventType match {
        case GuardTookShift(guardId) =>
          val newHistoryList = if (historyList.nonEmpty) {
            addEventToLastHistory(historyList, Event(moveTimeBackToFirstHourEnd(event.time), GuardEndedShift))
          } else {
            historyList
          }

          newHistoryList :+ Shift(guardId, Seq(event))
        case GuardFellAsleep | GuardWokeUp =>
          addEventToLastHistory(historyList, event)
      }
    }

    val sleepOnShiftsPerGuard = shifts.groupBy(_.guardId).mapValues(_.map(_.minutesAsleepAfterMidnight))

    val minuteSleepsPerGuard = sleepOnShiftsPerGuard.mapValues(sleepsPerMinute)

    val maxSleepPerGuard = minuteSleepsPerGuard.mapValues(_.values.toSeq.sorted.reverse.headOption.getOrElse(0))

    val maxGuard = maxSleepPerGuard.toSeq.maxBy(_._2)._1

    val maxGuardSleepMinutes = minuteSleepsPerGuard(maxGuard)

    val maxMinute = maxGuardSleepMinutes.toSeq.maxBy(_._2)._1

    println(maxGuard)
    println(maxMinute)
    println(maxGuard * maxMinute)
  }

  def sleepsPerMinute(minutesAsleepInShifts: Seq[MinutesAsleep]) = {
    minutesAsleepInShifts.foldLeft(Map[Minute, Int]().withDefaultValue(0)) { (map, minutesAsleep) =>
      val mapForCurrentShift = minutesAsleep.map(m => (m, map(m) + 1)).toMap.withDefaultValue(0)
      map ++ mapForCurrentShift
    }
  }

  def addEventToLastHistory(historyList: Seq[Shift], newEvent: Event) = {
    val last = historyList.last
    val newLast = last.append(newEvent)

    historyList.filterNot(_.eq(last)) :+ newLast
  }

  sealed trait EventType

  case class GuardTookShift(guardId: GuardId) extends EventType

  object GuardFellAsleep extends EventType

  object GuardWokeUp extends EventType

  object GuardEndedShift extends EventType

  object EventType {

    val GuardTookShiftRegex: Regex = """Guard #(\d*) begins shift""".r

    def parse(str: String): EventType = {
      str match {
        case GuardTookShiftRegex(guardId) => GuardTookShift(guardId.toLong)
        case "falls asleep" => GuardFellAsleep
        case "wakes up" => GuardWokeUp
      }
    }
  }

  case class Event(time: LocalDateTime, eventType: EventType)

  case class Shift(guardId: GuardId, events: Seq[Event]) {

    def append(e: Event) = {
      Shift(guardId, events :+ e)
    }

    def minutesAsleepAfterMidnight: MinutesAsleep = {
      def getMinuteAfterMidnight(time: LocalDateTime) = {
        if (time.getHour == 0) {
          time.getMinute
        } else {
          0
        }
      }

      val r = events.tail.foldLeft((events.head, Seq[Int]())) { (accumulator, event) =>
        (accumulator, event) match {
          case ((lastEvent, accumulatingMinutes), Event(time, eventType)) =>
            (lastEvent.eventType, eventType) match {
              case (GuardFellAsleep, GuardWokeUp) | (GuardFellAsleep, GuardEndedShift) =>
                val sleepMinutes = for {
                  m <- getMinuteAfterMidnight(lastEvent.time) until getMinuteAfterMidnight(event.time)
                } yield m

                (event, accumulatingMinutes ++ sleepMinutes)
              case (_, _) => (event, accumulator._2)
            }
        }
      }

      r._2
    }
  }
}


