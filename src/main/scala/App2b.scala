import scala.io.Source

object App2b {


  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input2.txt").getLines().toList

    lines.map { line =>
      val linesToCompare = lines.diff(Seq(line))

      linesToCompare.foreach { matchLine =>
        val c = commonLetters(line, matchLine)
        if (c.size == line.length - 1) {
          println(c.mkString(""))
        }
      }
    }
  }

  def diffFields(line: String, matchLine: String): Seq[Int] = {
    line.zipWithIndex.foldLeft(Seq[Int]())((diffFieldsSeq, t) => t match {
      case (currChar, currCharIdx) => if (matchLine.charAt(currCharIdx) == currChar) {
        diffFieldsSeq
      } else {
        diffFieldsSeq :+ currCharIdx
      }
    })
  }

  def commonLetters(line: String, matchLine: String) = {
    line.zipWithIndex.foldLeft(Seq[Char]())((commonLettersSeq, t) => t match {
      case (currChar, currCharIdx) => if (matchLine.charAt(currCharIdx) == currChar) {
        commonLettersSeq :+ currChar
      } else {
        commonLettersSeq
      }
    })
  }
}
