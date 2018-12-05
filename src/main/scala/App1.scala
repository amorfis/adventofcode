import scala.io.Source

object App1 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input1.txt").getLines().map(_.toLong)
    val result = lines.sum

    println(result)
  }

}
