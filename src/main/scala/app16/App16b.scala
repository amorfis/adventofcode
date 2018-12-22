package app16

import scala.io.Source

object App16b {

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

    val mapWithAll = Map[Int, Seq[Opcode.Creator[Opcode]]]().withDefaultValue(Opcode.allCreators)

    val numToOpcodes = runCases.map { runCase =>
      runCase.numberToMachingOpcodes
    }

    val mm = numToOpcodes.foldLeft(mapWithAll) {
      case (map, (opcodeNumber, matchingOpcodes)) =>
        val nonMatchingOpcodes = Opcode.allCreators diff matchingOpcodes

        val newMapValue = map(opcodeNumber) diff nonMatchingOpcodes

        map + (opcodeNumber -> newMapValue)
    }

    println(mm)

    val mapIt = Iterator.iterate(mm) { map =>
      val knownOpcodes = map.filter( p => p._2.size == 1).mapValues(_.head).values.toSeq
      map.mapValues { opcodes =>
        if (opcodes.size == 1) {
          opcodes
        } else {
          opcodes.filterNot(knownOpcodes.contains(_))
        }
      }
    }

    while(mapIt.next().values.exists(_.size > 1)) {}
    val semiFinalMap = mapIt.next()

    val finalMap = semiFinalMap.mapValues(_.head)

    val testCode = Source.fromResource("input16b.txt").getLines()
    val tr = """(\d+) (\d+) (\d+) (\d+)""".r
    val commands = testCode.map {
      case tr(o, a, b, c) => Seq(o.toInt, a.toInt, b.toInt, c.toInt)
    }

    val finalState = commands.foldLeft(Registers(Seq(0, 0, 0, 0))) { (regs, command) =>
      runCommand(regs, command, finalMap)
    }

    println(finalState)
  }

  def runCommand(reg: Registers, command: Seq[Int], opcodesMap: Map[Int, Opcode.Creator[Opcode]]) = {
    val opcode = opcodesMap(command.head).create(command.tail)
    opcode.run(reg)
  }
}
