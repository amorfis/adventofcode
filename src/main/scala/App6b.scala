import scala.collection.GenSeq
import scala.io.Source

object App6b {

  case class Coordinate(x: Long, y: Long)

  case class Circle(radius: Long, fieldsWithinLimit: Long)

  val DistancesSumLimit = 10000

  def main(args: Array[String]): Unit = {

    val coords: Seq[Coordinate] = Source.fromResource("input6.txt").getLines().map { c =>
      val pair = c.split(",")
      Coordinate(pair(0).trim.toLong, pair(1).trim.toLong)
    }.toSeq

    val calc = new AreaCalculator(coords)

    println(s"Middle: ${calc.middle}")
    println(calc.fieldsWithinLimit)
  }

  class AreaCalculator(coordinates: GenSeq[Coordinate]) {
    val minX = coordinates.map(_.x).min
    val maxX = coordinates.map(_.x).max
    val minY = coordinates.map(_.y).min
    val maxY = coordinates.map(_.y).max

    val middle = Coordinate((minX + maxX) / 2, (minY + maxY) / 2)

    val expandingCircles = circles(middle, 0).takeWhile(_.fieldsWithinLimit > 0)

    val fieldsWithinLimit = expandingCircles.map(_.fieldsWithinLimit).sum

    def distancesSum(c: Coordinate) = coordinates.map(manhattanDistance(c, _)).sum

    def circles(c: Coordinate, startFromDistance: Long): Stream[Circle] = {
      calculateCircle(c, startFromDistance) #:: circles(c, startFromDistance + 1)
    }

    def calculateCircle(c: Coordinate, radius: Long): Circle = {
      println(s"Calculating circle with radius $radius")

      val part1 = for {
        x <- (c.x - radius) to (c.x + radius)
        y <- Seq(c.y - radius, c.y + radius)
      } yield Coordinate(x, y)

      val part2 = for {
        x <- Seq(c.x - radius, c.x + radius)
        y <- (c.y - radius) to (c.y + radius)
      } yield Coordinate(x, y)

      val circleFields = part1.toSet ++ part2

      val distancesPerField = circleFields.toSeq.map(f => distancesSum(f))
      val fieldsWithinLimit = distancesPerField.count(_ < DistancesSumLimit)

      println(s"Fields within limit in circle: $fieldsWithinLimit")

      Circle(radius, fieldsWithinLimit)
    }
  }

  case class Field(c: Coordinate, tiedTo: Option[Coordinate])

  def manhattanDistance(l: Coordinate, r: Coordinate) = {
    Math.abs(l.x - r.x) + Math.abs(l.y - r.y)
  }
}


