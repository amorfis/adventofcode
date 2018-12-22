package app16

import scala.io.Source

object App16 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input16.txt").getLines().mkString("\n")
    val regex =
      ("""(?s)Before:\s*\[(\d+), (\d+), (\d+), (\d+)\]\n""" +
        """(\d+) (\d+) (\d+) (\d+)\n""" +
        """After:\s*\[(\d+), (\d+), (\d+), (\d+)\]""").r

    val runCases = regex.findAllMatchIn(lines).map { m =>
      val before = Seq(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt)
      val command = Seq(m.group(5).toInt, m.group(6).toInt, m.group(7).toInt, m.group(8).toInt)
      val after = Seq(m.group(9).toInt, m.group(10).toInt, m.group(11).toInt, m.group(12).toInt)

      RunCase(before, command, after)
    }

    val behaveLikeN = runCases.map { runCase =>
      val regBefore = Registers(runCase.before)

      val matchingOpcodes = Opcode.allCreators.map { creator =>
        val afterOpcode = creator.create(runCase.command.tail).run(regBefore)
        afterOpcode == Registers(runCase.after)
      }

      matchingOpcodes.count(identity)
    }.toList

    val r = behaveLikeN.count(_ >= 3)

    println(r)
  }
}
