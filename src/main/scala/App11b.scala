

object App11b {

//  val SerialNumber = 39
//  val SerialNumber = 2568

  case class Grid(width: Int, height: Int, serialNumber: Int) {

    var cellPowerCache = Map[(Int, Int), Int]()

    def powerAtCell(x: Int, y: Int) = {
      cellPowerCache.getOrElse((x, y), {

        if (x < 0 || x > width) {
          throw new RuntimeException(s"X out of range: $x")
        }

        if (y < 0 || y > height) {
          throw new RuntimeException(s"Y out of range: $y")
        }

        val rackId = x + 10
        val powerLevel = (rackId * y + serialNumber) * rackId
        val hDigit = powerLevel % 1000 / 100

        val r = hDigit - 5

        cellPowerCache = cellPowerCache + ((x, y) -> r)

        r
      })
    }

    def fieldPower(x: Int, y: Int, size: Int) = {
      val power = Field.fieldsCache.get(x, y, size - 1) match {
        case Some(smallPower) =>
          val xPower = (for {
            x <- x until x + size
          } yield {

            val p = powerAtCell(x, y + size - 1)
            p
          }).sum

          val yPower = (for {
            y <- y until y + size - 1
          } yield powerAtCell(x + size - 1, y)).sum

          //val bottomRight = powerAtCell(x + size - 1, y + size - 1)
          val p = smallPower + xPower + yPower// + bottomRight

//          val nc = calculatePowerNoCache(x, y, size)
//
//          if (p != nc) {
//            for {
//              y <- 1 to size + 1
//              x <- 1 to size + 1
//            } {
//              if (x == 1) println()
//              print(powerAtCell(x, y) + " ")
//            }
//            println()
//
//            throw new RuntimeException(s"Different power! Field ($x, $y, $size) No cache: $nc. Cache: $p")
//          }

          p
        case None =>
          calculatePowerNoCache(x, y, size)
      }

      Field.fieldsCache = Field.fieldsCache + ((x, y, size) -> power)

      power
    }

    def calculatePowerNoCache(x: Int, y: Int, size: Int) = {
      (for {
        x <- x until x + size
        y <- y until y + size
      } yield powerAtCell(x, y))
        .sum
    }

    def maxPowerField(size: Int) = {

      val start = System.currentTimeMillis()

      val fields = for {
        x <- 1 to width - size + 1
        y <- 1 to height - size + 1
      } yield {
//        val inStart = System.nanoTime()
        val f = Field(x, y, size, fieldPower(x, y, size))
//        val inEnd = System.nanoTime()
//        println(s"Checking field $x, $y of size $size took ${inEnd - inStart} nanos")
        f
      }

      val end = System.currentTimeMillis()

      println(s"Checking field of size $size took ${end - start} millis")

      fields.maxBy(_.power)
    }
  }

  case class Field(x: Int, y: Int, size: Int, power: Int)

  object Field {

    var fieldsCache = Map[(Int, Int, Int), Int]()
  }

  def main(args: Array[String]): Unit = {

    val Size = 300
    val grid = Grid(Size, Size, 2568)

//    println(s"Test power: ${grid.fieldPower(1, 3, 2)}")




    val bestFields = for {
      i <- 1 to Size
    } yield {
      println(s"Looking for best of size: $i")
      grid.maxPowerField(i)
    }

    println(bestFields.maxBy(_.power))

    println(Field.fieldsCache.get((1, 3, 2)))
  }
}


