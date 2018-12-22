package app18

case class State(forest: Map[Pos, Field], rev: Int) {

  def next() = {

    val newForest = forest.keySet.map(p => p -> transform(p)).toMap
    State(newForest, rev + 1)
  }

  def transform(pos: Pos): Field = {
    val adjacent = pos.adjacent.flatMap(forest.get)

    forest(pos) match {
      case Space =>
        if (adjacent.count(_.isTree) >= 3) {
          Tree
        } else {
          Space
        }

      case Tree =>
        if (adjacent.count(_.isLumberyard) >= 3) {
          Lumberyard
        } else {
          Tree
        }

      case Lumberyard =>
        if (adjacent.count(_.isLumberyard) >= 1 && adjacent.count(_.isTree) >= 1) {
          Lumberyard
        } else {
          Space
        }
    }
  }

  override def toString() = {
    println(rev)

    val lines = for {
      y <- 0 to forest.keySet.map(_.y).max
    } yield {
      val line = for {
        x <- 0 to forest.keySet.map(_.x).max
      } yield {
        forest(Pos(x, y)) match {
          case Space => "."
          case Tree => "|"
          case Lumberyard => "#"
        }
      }

      line.mkString("")
    }

    lines.mkString("\n")
  }

}
