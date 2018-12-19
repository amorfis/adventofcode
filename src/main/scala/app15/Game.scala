package app15

import java.util.UUID

import app15.App15.{Board, FighterTurnResult, fightersOrdering}

case class Game(board: Board, elves: Seq[Elf], goblins: Seq[Goblin], roundsCount: Int = 0) {

  case class Subturn(elvesTurned: Seq[Elf], goblinsTurned: Seq[Goblin], elvesToTurn: Seq[Elf], goblinsToTurn: Seq[Goblin]) {

    def moveToTurned(before: Fighter, after: Fighter): Subturn = {
      (before, after) match {
        case (b: Elf, a: Elf) => Subturn(elvesTurned :+ a, goblinsTurned, elvesToTurn.filterNot(_ == b), goblinsToTurn)
        case (b: Goblin, a: Goblin) => Subturn(elvesTurned, goblinsTurned :+ a, elvesToTurn, goblinsToTurn.filterNot(_ == b))
      }
    }
  }

  def fighterTurn(subturn: Subturn) = {

    def modifyFighterAttacked(before: Fighter, after: Fighter) = {
      (before, after) match {
        case (b: Goblin, a: Goblin) =>

          val (nextGTurned, nextGToTurn) =
            if (subturn.goblinsTurned.contains(b)) {

              val nextGoblinsTurned =
                if (a.hp > 0) {
                  subturn.goblinsTurned.filterNot(_ == b) :+ a
                } else {
                  subturn.goblinsTurned.filterNot(_ == b)
                }

            (nextGoblinsTurned, subturn.goblinsToTurn)
          } else if (subturn.goblinsToTurn.contains(b)) {
            val nextGoblinsToTurn =
              if (a.hp > 0) {
                subturn.goblinsToTurn.filterNot(_ == b) :+ a
              } else {
                subturn.goblinsToTurn.filterNot(_ == b)
              }

            (subturn.goblinsTurned, nextGoblinsToTurn)
          } else {
            throw new RuntimeException("No i dupa")
          }

          Subturn(subturn.elvesTurned, nextGTurned, subturn.elvesToTurn, nextGToTurn)

        case (b: Elf, a: Elf) =>
          val (nextETurned, nextEToTurn) =
            if (subturn.elvesTurned.contains(b)) {

              val nextElvesTurned =
                if (a.hp > 0) {
                  subturn.elvesTurned.filterNot(_ == b) :+ a
                } else {
                  subturn.elvesTurned.filterNot(_ == b)
                }

              (nextElvesTurned, subturn.elvesToTurn)
            } else if (subturn.elvesToTurn.contains(b)) {
              val nextElvesToTurn =
                if (a.hp > 0) {
                  subturn.elvesToTurn.filterNot(_ == b) :+ a
                } else {
                  subturn.elvesToTurn.filterNot(_ == b)
                }

              (subturn.elvesTurned, nextElvesToTurn)
            } else {
              throw new RuntimeException("No i dupa")
            }

          Subturn(nextETurned, subturn.goblinsTurned, nextEToTurn, subturn.goblinsToTurn)
      }
    }

    (subturn.elvesToTurn ++ subturn.goblinsToTurn).sorted(fightersOrdering).headOption match {

      case Some(fighter) =>
        val currentGoblins = subturn.goblinsToTurn ++ subturn.goblinsTurned
        val currentElves = subturn.elvesToTurn ++ subturn.elvesTurned
        fighter match {
          case f: Elf =>
            f.doTurn(board, currentGoblins, currentElves) match {

              case FighterTurnResult(maybeMove, maybeAttack) =>

                val nextSubturn = maybeAttack.map {
                  case (enemyBefore: Goblin, enemyAfter: Goblin) =>
                    modifyFighterAttacked(enemyBefore, enemyAfter)
                }
                  .getOrElse(subturn)

                val reallyNextSubturn = maybeMove.map { newP =>
                  Elf(f.hp, newP)
                }
                  .map(movedElf => nextSubturn.moveToTurned(f, movedElf))
                  .getOrElse(nextSubturn.moveToTurned(f, f))

                reallyNextSubturn
            }
          case f: Goblin =>
            f.doTurn(board, currentElves, currentGoblins) match {

              case FighterTurnResult(maybeMove, maybeAttack) =>

                val nextSubturn = maybeAttack.map {
                  case (enemyBefore: Elf, enemyAfter: Elf) =>
                    modifyFighterAttacked(enemyBefore, enemyAfter)
                }
                  .getOrElse(subturn)

                val reallyNextSubturn = maybeMove.map { newP =>
                  Goblin(f.hp, newP)
                }
                  .map(movedGoblin => nextSubturn.moveToTurned(f, movedGoblin))
                  .getOrElse(nextSubturn.moveToTurned(f, f))

                reallyNextSubturn
            }
      }
    }

//    case f if killed.contains(f.uuid) => (currentElves, currentGoblins)

  }

  def round(): Game = {

    def doSubturns(subturn: Subturn): Subturn = {
      if ((subturn.goblinsToTurn ++ subturn.elvesToTurn).isEmpty) {
        subturn
      } else {
        doSubturns(fighterTurn(subturn))
      }
    }

    val finalSubturn = doSubturns(Subturn(Seq(), Seq(), elves, goblins))

//    val nextFighters = (elves ++ goblins).sorted(fightersOrdering).foldLeft((elves, goblins)) {
//      case ((currentElves, currentGoblins), fighter) => fighter match {
//        case f if killed.contains(f.uuid) => (currentElves, currentGoblins)
//        case f: Elf =>
//          f.doTurn(board, currentGoblins, currentElves) match {
//
//            case FighterTurnResult(maybeMove, maybeAttack) =>
//
//              val nextGoblins = maybeAttack.map {
//                case (enemyBefore: Goblin, enemyAfter: Goblin) =>
//                  if (enemyAfter.hp > 0) {
//                    (currentGoblins diff Seq(enemyBefore)) :+ enemyAfter
//                  } else {
//                    currentGoblins diff Seq(enemyBefore)
//                  }
//              }
//                .getOrElse(currentGoblins)
//
//              val nextElves = maybeMove.map { newP =>
//                val newElf = Elf(f.hp, newP)
//                (currentElves diff Seq(f)) :+ newElf
//              }
//                .getOrElse(currentElves)
//
//              (nextElves, nextGoblins)
//          }
//        case f: Goblin =>
//          f.doTurn(board, currentElves, currentGoblins) match {
//
//            case FighterTurnResult(maybeMove, maybeAttack) =>
//
//              val nextElves = maybeAttack.map {
//                case (enemyBefore: Elf, enemyAfter: Elf) =>
//                  if (enemyAfter.hp > 0) {
//                    (currentElves diff Seq(enemyBefore)) :+ enemyAfter
//                  } else {
//                    currentElves diff Seq(enemyBefore)
//                  }
//              }
//                .getOrElse(currentElves)
//
//              val nextGoblins = maybeMove.map { newP =>
//                val newGoblin = Goblin(f.hp, newP)
//                (currentGoblins diff Seq(f)) :+ newGoblin
//              }
//                .getOrElse(currentGoblins)
//
//              (nextElves, nextGoblins)
//          }
//      }
//    }

    Game(board, finalSubturn.elvesTurned, finalSubturn.goblinsTurned, roundsCount + 1)
  }

  override def toString: String = {
    val lines = for {
      y <- 0 to board.bottomRight._2
    } yield {
      val line = for {
        x <- 0 to board.bottomRight._1
      } yield {
        elves.find(_.position == (x, y)).map(_ => "E").getOrElse(
          goblins.find(_.position == (x, y)).map(_ => "G").getOrElse(
            board.area(x, y).toString
          )
        )
      }

      val boardLine = line.mkString("")

      val lineFighters = (elves ++ goblins).filter(_.position._2 == y)
      val hpString = lineFighters.sortBy(_.position._1).map("(" + _.hp + ")").mkString(" ")

      boardLine + "   " + hpString
    }

    lines.mkString("\n")
  }
}
