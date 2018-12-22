package app17

case class Pos(x: Int, y: Int) {

  def isAdjacent(other: Pos) = {
    if (x == other.x) {
      y == other.y + 1 || y == other.y - 1
    } else if (y == other.y) {
      x == other.x + 1 || x == other.x - 1
    } else {
      false
    }
  }

  lazy val below = Pos(x, y + 1)
  lazy val right = Pos(x + 1, y)
  lazy val left = Pos(x - 1, y)
}
