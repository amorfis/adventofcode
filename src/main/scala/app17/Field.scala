package app17

trait Field {
  val isPermeable: Boolean
}

trait Water extends Field {
  val isFlowing: Boolean
}

case class Sand(pos: Pos) extends Field {
  override val isPermeable: Boolean = true
}

object FlowingWater extends Water {
  override val isPermeable: Boolean = true
  override val isFlowing = true
}

object StableWater extends Water {
  override val isPermeable: Boolean = false
  override val isFlowing = false
}

case class Clay(pos: Pos) extends Field {
  override val isPermeable: Boolean = false
}

object Clay {

  def apply(x: Int, yr: Range): Seq[Clay] = {
    for {
      y <- yr
    } yield Clay(Pos(x, y))
  }

  def apply(xr: Range, y: Int): Seq[Clay] = {
    for {
      x <- xr
    } yield Clay(Pos(x, y))
  }
}
