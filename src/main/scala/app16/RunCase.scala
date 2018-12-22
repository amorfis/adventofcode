package app16

case class RunCase(before: Seq[Int], command: Seq[Int], after: Seq[Int]) {

  val numberToMachingOpcodes = {
    val matchingOpcodes: Seq[Opcode.Creator[_]] = Opcode.allCreators.map { creator =>
      val afterOpcode = creator.create(command.tail).run(Registers(before))

      if (afterOpcode == Registers(after)) {
        Some(creator)
      } else {
        None
      }
    }.collect {
      case Some(oc) => oc
    }

    command.head -> matchingOpcodes
  }
}
