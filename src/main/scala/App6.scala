import scala.collection.{GenSeq, immutable}
import scala.collection.parallel.ParSeq
import scala.io.Source

object App6 {

  case class Coordinate(x: Long, y: Long)

  def main(args: Array[String]): Unit = {

    val coords: Seq[Coordinate] = Source.fromResource("input6.txt").getLines().map { c =>
      val pair = c.split(",")
      Coordinate(pair(0).trim.toLong, pair(1).trim.toLong)
    }.toSeq

    val calc = new AreaCalculator(coords)

    println(calc.maxArea)
    println(calc.maxArea._2.size)
  }

  class AreaCalculator(coordinates: GenSeq[Coordinate]) {
    val maxDistance = (for {
      c <- coordinates
      subc <- coordinates
    } yield manhattanDistance(c, subc))
      .max

    println(s"Max distance is $maxDistance")

    val minX = coordinates.map(_.x).min - maxDistance
    val maxX = coordinates.map(_.x).max + maxDistance
    val minY = coordinates.map(_.y).min - maxDistance
    val maxY = coordinates.map(_.y).max + maxDistance

    println(s"World is ($minX, $minY) to ($maxX, $maxY)")

    val world = for {
      x <- minX to maxX
      y <- minY to maxY
    } yield Coordinate(x, y)

    val fields = world.map(tieField)


    // remove infinite areas. Infinite ones are the ones that touch the border
    val filteredFields = removeAreasTouchingBorder(fields.filter(_.tiedTo.isDefined))

    val areasByBase = filteredFields.groupBy(_.tiedTo.get)

    val maxArea = areasByBase.toList.maxBy(_._2.size)

    def tieField(fieldCoord: Coordinate) = {
      val distances = coordinates.map(c => (manhattanDistance(fieldCoord, c), c))
      val minDistance = distances.minBy(_._1)._1

      val possibleFields = distances.filter(_._1 == minDistance)
      if (possibleFields.size > 1) {
        Field(fieldCoord, None)
      } else {
        Field(fieldCoord, Some(possibleFields.head._2))
      }
    }

    def removeAreasTouchingBorder(fields: Seq[Field]) = {
      val borderFieldsOwners = fields.filter(f => isOnBorder(f.c)).map(_.tiedTo)
      fields.filterNot(f => borderFieldsOwners.contains(f.tiedTo))
    }

    def isOnBorder(c: Coordinate) = c.x == minX || c.x == maxX || c.y == minY || c.y == maxY
  }

  case class Field(c: Coordinate, tiedTo: Option[Coordinate])

  def manhattanDistance(l: Coordinate, r: Coordinate) = {
    Math.abs(l.x - r.x) + Math.abs(l.y - r.y)
  }
}


