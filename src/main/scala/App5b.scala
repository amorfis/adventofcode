import scala.io.Source

object App5b {

  def main(args: Array[String]): Unit = {
    val polymer = Source.fromResource("input5.txt").toList.mkString

    val allUnitTypes = polymer.foldLeft(Set[Char]()) { (set, currentChar) =>
      set + currentChar.toLower
    }

    val processedFilteredPolymers = allUnitTypes.map { unit =>
      val filtered = polymer.filter(_.toLower != unit)
      (unit, processPolymer(filtered))
    }

    val sorted = processedFilteredPolymers.toSeq.sortBy(_._2.length)

    println(sorted.head)
    println(sorted.head._2.length)
  }

  def processPolymer(polymer: String): String = {
    val processed = processPolymerOnce(polymer.toList).mkString

    if (processed.length == polymer.length) {
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


