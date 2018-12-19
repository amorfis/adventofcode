package app15b

import java.util.UUID

import app15.App15.{Attack, Board, Enemy, FighterTurnResult, Position, fightersOrdering, positionOrdering}

import scala.collection.Set
import App15.toRichPosition

sealed trait Fighter  {
  val hp: Int
  val position: Position

  def isInRange(position: Position) = {
    (this.position._1 - position._1, this.position._2 - position._2) match {
      case (1, 0) | (-1, 0) | (0, 1) | (0, -1) => true
      case (0, 0) => throw new RuntimeException
      case _ => false
    }
  }

  def doTurn(board: Board, enemies: Seq[Enemy], friends: Seq[Fighter]): FighterTurnResult = {
    val move = tryMove(board, enemies, friends)

    val attackPosition = move.getOrElse(position)
    val attackResult = tryAttack(attackPosition, enemies)

    FighterTurnResult(move, attackResult)
  }

  def tryMove(board: Board, enemies: Seq[Enemy], friends: Seq[Fighter]) = {
    val inRange = enemies.flatMap(_.adjacentFreeSpaces(board, enemies)).toSet
    if (inRange.contains(position)) {
      None
    } else {
      adjacentFreeSpaces(board, enemies ++ friends) match {
        case Nil => None
        case possibleMoves =>
          val reachableWithDistances = calculateDistancesToReachable(position, board, (enemies ++ friends).map(_.position))

          val possibleDestinations = reachableWithDistances.filter(rd => inRange.contains(rd._1))

          if (possibleDestinations.isEmpty) {
            None
          } else {
            val minDistance = possibleDestinations.minBy(_._2)._2
            val destination = possibleDestinations.filter(_._2 == minDistance).keys.toSeq.sorted(positionOrdering).head

            if (possibleMoves.contains(destination)) {
              Some(destination)
            } else {
              val distancesFromDestination = calculateDistancesToReachable(destination, board, (enemies ++ friends).map(_.position))

              val possibleMovesWithDistance = distancesFromDestination.filter(dd => possibleMoves.contains(dd._1))
              val minStepDistance = possibleMovesWithDistance.values.min
              val nextStep = possibleMovesWithDistance.filter(_._2 == minStepDistance).keys.toSeq.sorted(positionOrdering).head

              Some(nextStep)
            }
          }
      }
    }
  }

  def findReachable(startingPoint: Position, board: Board, occupied: Seq[Position]) = {

    def addToReachable(startingPoints: Set[Position], board: Board, occupied: Seq[Position], reachableUpToNow: Seq[Position]): Set[Position] = {
      val reachable = startingPoints.flatMap(_.adjacentFreeSpaces(board, occupied))
      val newReachable = reachable.filterNot(reachableUpToNow.contains(_))
      newReachable match {
        case s if s.isEmpty => reachable
        case s => addToReachable(s, board, occupied, reachableUpToNow ++ s)
      }
    }

    addToReachable(Set(startingPoint), board, occupied, Seq())
  }

  def calculateDistancesToReachable(startingPoint: Position, board: Board, occupied: Seq[Position]) = {

    def addToCalculated(startingPoints: Map[Position, Int], board: Board, occupied: Seq[Position], calculatedUpToNow: Map[Position, Int]): Map[Position, Int] = {
      val maybeNewPoints = startingPoints.flatMap(pd => pd._1.adjacentFreeSpaces(board, occupied).map(ap => (ap, pd._2 + 1)))
      val newPoints = maybeNewPoints.filterKeys(nspk => !calculatedUpToNow.keySet.contains(nspk))
      if (newPoints.isEmpty) {
        calculatedUpToNow
      } else {
        addToCalculated(newPoints, board, occupied, calculatedUpToNow ++ newPoints)
      }


      //        startingPoints.foldLeft(calculatedUpToNow) {
      //          case (calculated, (currentPoint, currentPointDistance)) =>
      //            val newPositions = currentPoint.adjacentFreeSpaces(board, occupied).filterNot(p => calculated.keySet.contains(p))
      //            if (newPositions.isEmpty) {
      //              calculated
      //            } else {
      //              val newPoints = newPositions.map(p => p -> (currentPointDistance + 1)).toMap
      //              val newCalculated = calculated ++ newPoints
      //              addToCalculated(newPoints, board, occupied, newCalculated)
      //            }
      //        }
    }

    def drawDistance(points: Map[Position, Int]) = {
      val lines = for {
        y <- 0 to board.bottomRight._2
      } yield {
        val line = for {
          x <- 0 to board.bottomRight._1
        } yield {
          points.find(_._1 == (x, y)).map(p => if (p._2 > 9) "m" else p._2).getOrElse(
            board.area(x, y).toString)
        }

        line.mkString("")
      }

      lines.mkString("\n")
    }

    val dr = addToCalculated(Map(startingPoint -> 0), board, occupied, Map())

    //      println(drawDistance(dr))

    dr
  }

  def adjacentFreeSpaces(board: Board, fighters: Seq[Fighter]) = adjacentFreeSpacesP(board, fighters.map(_.position): Seq[Position])
  def adjacentFreeSpacesP(board: Board, occupiedSpaces: Seq[Position]) = position.adjacentFreeSpaces(board, occupiedSpaces)

  def tryAttack(attackPosition: Position, enemies: Seq[Fighter]): Option[(Fighter, Fighter)] = {
    val closeEnemies = enemies.filter(_.isInRange(attackPosition))

    val maybeEnemy = if (closeEnemies.isEmpty) {
      None
    } else {
      val minHp = closeEnemies.map(_.hp).min
      closeEnemies.filter(_.hp == minHp).sorted(fightersOrdering).headOption
    }

    maybeEnemy.map(e => (e, attack(e)))
  }

  def attack(enemy: Fighter): Fighter = {
    enemy.takeHit(Attack)
  }

  def takeHit(hpToSubtract: Int): Fighter
}

case class Elf(hp: Int, position: Position) extends Fighter {
  override def takeHit(hpToSubtract: Int): Elf = this.copy(hp = this.hp - hpToSubtract)
}

case class Goblin(hp: Int, position: Position) extends Fighter {
  override def takeHit(hpToSubtract: Int): Goblin = this.copy(hp = this.hp - hpToSubtract)
}