import scala.io.Source

object App11 {

//  val SerialNumber = 39
//  val SerialNumber = 2568

  case class Grid(width: Int, height: Int, serialNumber: Int) {

    def powerAtCell(x: Int, y: Int) = {
      if (x < 0 || x > width) {
        throw new RuntimeException("X out of range")
      }

      if (y < 0 || y > height) {
        throw new RuntimeException("Y out of range")
      }

      val rackId = x + 10
      val powerLevel = (rackId * y + serialNumber) * rackId
      val hDigit = powerLevel % 1000 / 100

      hDigit - 5
    }
  }

  case class Field(x: Int, y: Int, grid: Grid) {

    val fieldPower =
      (for {
        x <- x to x + 2
        y <- y to y + 2
      } yield grid.powerAtCell(x, y))
        .sum
  }

  def main(args: Array[String]): Unit = {

    val grid = Grid(300, 300, 2568)

    val fields = for {
      x <- 1 to 300 - 2
      y <- 1 to 300 - 2
    } yield {
      Field(x, y, grid)
    }

    val bestField = fields.sortBy(_.fieldPower).last

    println(bestField)
    println(bestField.fieldPower)
  }
}


