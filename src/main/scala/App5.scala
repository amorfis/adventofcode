import java.text.SimpleDateFormat
import java.time._
import java.util.TimeZone

import scala.io.Source
import scala.util.matching.Regex

object App5 {

  def main(args: Array[String]): Unit = {
    val polymer = Source.fromResource("input5.txt").toList.mkString

    println(processPolymer("dDabAcCaCBAcCcaDAa"))
    // abAaCBAcaD
    // abCBAcaD

    val processed = processPolymer(polymer)
    println(processed)
    println(processed.size)
  }

  def processPolymer(polymer: String): String = {
    val processed = processPolymerOnce(polymer.toList).mkString

    if (processed.size == polymer.size) {
      processed
    } else {
      processPolymer(processed)
    }
  }

  def processPolymerOnce(polymer: List[Char]) = {
    val withNullHead = null.asInstanceOf[Char] :: polymer
    val withNullLast = polymer.drop(1) :+ null.asInstanceOf[Char]
    val zippedWithPrevAndNext = withNullHead.zip(polymer).zip(withNullLast)
    // This omits first and last element, we need to handle them later

    val processed = zippedWithPrevAndNext.map {
      case ((prev, current), next) if shouldAnnihilate(prev, current) && shouldAnnihilate(current, next) => Some(prev)
      case ((prev, current), next) if shouldAnnihilate(prev, current) || shouldAnnihilate(current, next) => None
      case ((_, current), _) => Some(current)
    }.collect {
      case Some(char) => char
    }

    processed
  }

  def shouldAnnihilate(l: Char, r: Char) = {
    (l != r) && (l.toUpper == r.toUpper)
  }
}


