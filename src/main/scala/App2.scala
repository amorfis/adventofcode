import scala.io.Source

object App2 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input2.txt").getLines()

    val _2and3 = lines.foldLeft((0, 0)) { (tuple, line) =>
      val exactly2 = if (lettersHistogram(line).values.toSeq.contains(2)) {
        tuple._1 + 1
      } else {
        tuple._1
      }

      val exactly3 = if (lettersHistogram(line).values.toSeq.contains(3)) {
        tuple._2 + 1
      } else {
        tuple._2
      }

      (exactly2, exactly3)
    }

    println(_2and3._1 * _2and3._2)
  }

  def lettersHistogram(line: String) = {
    line.toCharArray.foldLeft(Map[Char, Int]().withDefaultValue(0)) { (histogram, letter) =>
      println(histogram)
      histogram + (letter -> (histogram(letter) + 1))
    }
  }

}
