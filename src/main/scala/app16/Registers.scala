package app16

case class Registers(regs: Seq[Int]) {
  if (regs.size != 4) {
    throw new RuntimeException
  }
}
