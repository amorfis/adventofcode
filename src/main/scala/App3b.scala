import scala.collection.immutable.TreeSet
import scala.io.Source

object App3b {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("input3.txt")
    val lines = source.getLines().toList
    val patches = lines.map { line =>
      val r = """#(\d*) @ (\d*),(\d*): (\d*)x(\d*)""".r
      line match {
        case r(id, left, top, width, height) => Fabric(id.toLong, left.toLong, top.toLong, width.toLong, height.toLong)
      }
    }

    val allPoints = patches.flatMap(_.points)

    type Point = (Long, Long)

    val occupation = allPoints.foldLeft(Map[Point, Long]().withDefaultValue(0L))((map, point) => map + (point -> (map(point) + 1L)))

    val overlappingPoints = occupation.filter {
      case (_, claims) => claims > 1
    }.keys.toSeq

    val fastOverlappingPoints = TreeSet(overlappingPoints:_*)

    val nonOverlappingPatches = patches.filterNot { patch =>
      println(s"Checking patch $patch")
      patch.points.exists(patchPoint => fastOverlappingPoints.contains(patchPoint))
    }

    println(nonOverlappingPatches)
  }


  case class Fabric(id: Long, left: Long, top: Long, width: Long, height: Long) {

    def points = {
      for {
        fromLeft <- left to (left + width - 1)
        fromTop <- top to (top + height - 1)
      } yield {
        (fromLeft, fromTop)
      }
    }
  }
}
