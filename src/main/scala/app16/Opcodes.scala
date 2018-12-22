package app16

trait Opcode {
  val a, b, c: Int

  def run(registers: Registers): Registers = {
    val r = result(registers.regs(a), registers.regs(b))
    Registers(registers.regs.updated(c, r))
  }

  def result(regA: Int, regB: Int): Int
}

object Opcode {

  val allCreators = Seq(
    AddrCreator,
    AddiCreator,
    MulrCreator,
    MuliCreator,
    BanrCreator,
    BaniCreator,
    BorrCreator,
    BoriCreator,
    SetrCreator,
    SetiCreator,
    GtirCreator,
    GtriCreator,
    GtrrCreator,
    EqirCreator,
    EqriCreator,
    EqrrCreator
  )

  trait Creator[+O <: Opcode] {
    def create(a: Int, b: Int, c: Int): O

    def create(io: Seq[Int]): O = {
      if (io.size != 3) throw new RuntimeException

      create(io(0), io(1), io(2))
    }
  }

  object AddrCreator extends Creator[Addr] {
    def create(a: Int, b: Int, c: Int) = Addr(a, b, c)
  }

  object AddiCreator extends Creator[Addi] {
    def create(a: Int, b: Int, c: Int) = Addi(a, b, c)
  }

  object MulrCreator extends Creator[Mulr] {
    def create(a: Int, b: Int, c: Int) = Mulr(a, b, c)
  }

  object MuliCreator extends Creator[Muli] {
    def create(a: Int, b: Int, c: Int) = Muli(a, b, c)
  }

  object BanrCreator extends Creator[Banr] {
    def create(a: Int, b: Int, c: Int) = Banr(a, b, c)
  }

  object BaniCreator extends Creator[Bani] {
    def create(a: Int, b: Int, c: Int) = Bani(a, b, c)
  }

  object BorrCreator extends Creator[Borr] {
    def create(a: Int, b: Int, c: Int) = Borr(a, b, c)
  }

  object BoriCreator extends Creator[Bori] {
    def create(a: Int, b: Int, c: Int) = Bori(a, b, c)
  }

  object SetrCreator extends Creator[Setr] {
    def create(a: Int, b: Int, c: Int) = Setr(a, b, c)
  }

  object SetiCreator extends Creator[Seti] {
    def create(a: Int, b: Int, c: Int) = Seti(a, b, c)
  }

  object GtirCreator extends Creator[Gtir] {
    def create(a: Int, b: Int, c: Int) = Gtir(a, b, c)
  }

  object GtriCreator extends Creator[Gtri] {
    def create(a: Int, b: Int, c: Int) = Gtri(a, b, c)
  }

  object GtrrCreator extends Creator[Gtrr] {
    def create(a: Int, b: Int, c: Int) = Gtrr(a, b, c)
  }

  object EqirCreator extends Creator[Eqir] {
    def create(a: Int, b: Int, c: Int) = Eqir(a, b, c)
  }

  object EqriCreator extends Creator[Eqri] {
    def create(a: Int, b: Int, c: Int) = Eqri(a, b, c)
  }

  object EqrrCreator extends Creator[Eqrr] {
    def create(a: Int, b: Int, c: Int) = Eqrr(a, b, c)
  }

  case class Addr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA + regB
  }

  case class Addi(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA + b
  }

  case class Mulr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA * regB
  }

  case class Muli(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA * b
  }

  case class Banr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA & regB
  }

  case class Bani(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA & b
  }

  case class Borr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA | regB
  }

  case class Bori(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA | b
  }

  case class Setr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = regA
  }

  case class Seti(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = a
  }

  case class Gtir(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = if (a > regB) 1 else 0

  }

  case class Gtri(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = if (regA > b) 1 else 0
  }

  case class Gtrr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = if (regA > regB) 1 else 0
  }

  case class Eqir(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = if (a == regB) 1 else 0
  }

  case class Eqri(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = if (regA == b) 1 else 0
  }

  case class Eqrr(a: Int, b: Int, c: Int) extends Opcode {
    override def result(regA: Int, regB: Int): Int = if (regA == regB) 1 else 0
  }
}
