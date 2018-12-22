package app18

case class Pos(x: Int, y: Int) {

  def adjacent = {
    Seq(
      Pos(x - 1, y - 1),
      Pos(x - 1, y),
      Pos(x - 1, y + 1),
      Pos(x, y - 1),
      Pos(x, y + 1),
      Pos(x + 1, y - 1),
      Pos(x + 1, y),
      Pos(x + 1, y + 1),
    )
  }
}

