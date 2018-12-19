package app15b

import scala.io.Source

object App15b {

  val InitialHp = 200

  type Position = (Int, Int)
  type Path = Seq[Position]

  case class Board(area: Map[Position, Field]) {
    val bottomRight: Position = {
      if (area.isEmpty) {
        (-1, -1)
      } else {
        area.keys.max
      }
    }
  }

  sealed trait Field {
    def isSpace: Boolean
  }

  object Rock extends Field {
    override def isSpace: Boolean = false
    override def toString: String = "#"
  }

  object Space extends Field {
    override def isSpace: Boolean = true
    override def toString: String = "."
  }

  implicit def toRichPosition(position: Position) = new {

    def adjacentFreeSpaces(board: Board, occupiedSpaces: Seq[Position]) = {
      val left = if (position._1 > 0) Some((position._1 - 1, position._2)) else None
      val right = if (position._1 < board.bottomRight._1 - 1) Some((position._1 + 1, position._2)) else None
      val top = if (position._2 > 0) Some((position._1, position._2 - 1)) else None
      val bottom = if (position._2 < board.bottomRight._2 - 1) Some((position._1, position._2 + 1)) else None

      Seq(top, left, right, bottom).collect {
        case Some(f) => f
      }
        .filter(board.area(_).isSpace)
        .filterNot(occupiedSpaces.contains(_))
    }

    def distance(field: Position) = Math.abs(position._1 - field._1) + Math.abs(position._2 - field._2)
  }

  implicit val positionOrdering: Ordering[Position] = new Ordering[Position]() {
    val intOrd = implicitly[Ordering[Int]]

    def compare(x: Position, y: Position): Int = {
      intOrd.compare(x._2, y._2) * 10 + intOrd.compare(x._1, y._1)
    }
  }

  implicit val fightersOrdering: Ordering[Fighter] = (x: Fighter, y: Fighter) => {
    positionOrdering.compare(x.position, y.position)
  }

  type Enemy = Fighter
  case class FighterTurnResult(move: Option[Position], attackedEnemy: Option[(Fighter, Fighter)])

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input15.txt").getLines()

    val initGame = lines.zipWithIndex.foldLeft(Game(Board(Map()), Seq(), Seq())) {
      case (game, (line, lineIdx)) =>
        line.zipWithIndex.foldLeft(game) {
          case (innerGame, (char, charIdx)) =>
            char match {
              case '#' =>
                val oldBoard = innerGame.board.area
                val newArea = oldBoard + ((charIdx, lineIdx) -> Rock)

                innerGame.copy(board = Board(newArea))
              case '.' => innerGame.copy(board = Board(innerGame.board.area + ((charIdx, lineIdx) -> Space)))
              case 'E' => innerGame.copy(
                board = Board(innerGame.board.area + ((charIdx, lineIdx) -> Space)),
                elves = innerGame.elves :+ Elf(InitialHp, (charIdx, lineIdx)))
              case 'G' => innerGame.copy(
                board = Board(innerGame.board.area + ((charIdx, lineIdx) -> Space)),
                goblins = innerGame.goblins :+ Goblin(InitialHp, (charIdx, lineIdx)))
            }
        }
    }

    def rounds(game: Game): Game = {
      println(s"After round ${game.roundsCount}")
      println(game)
      println()

      val nextGame = game.round()

      if (game == nextGame.copy(roundsCount = game.roundsCount)) {
        game
      } else {
        rounds(nextGame)
      }
    }

    val lastGame = rounds(initGame)
    val remainingHpSum = lastGame.elves.map(_.hp).sum + lastGame.goblins.map(_.hp).sum

    println(s"Last round: ${lastGame.roundsCount}")
    println(s"Remaining HP sum: $remainingHpSum")
    println(s"Multiply: ${remainingHpSum * lastGame.roundsCount}")
    println(s"Multiply prev round: ${remainingHpSum * (lastGame.roundsCount - 1)}")
  }
}
