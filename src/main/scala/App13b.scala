import scala.io.Source

object App13b {

  type Position = (Int, Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input13.txt").getLines().toIterable
    val tracks = mapPosition(lines, {
      case (p, '<' | '>') => '-'
      case (p, '^' | 'v' | 'V') => '|'
      case (p, k) if !k.isWhitespace => k
    })

    val carts = mapPosition(lines, {
      case (p, c @ ('<' | '>' | 'v' | 'V' | '^')) => Cart(c, p, 0)
    }).values.toList

    val state = State(tracks, carts, 0)

    val finalState = rollState(state)

    println(finalState.carts)
  }

  def rollState(state: State): State = {
    println(state)
    println(s"Carts: ${state.carts.size}")
    if (state.carts.size > 1) {
      rollState(state.nextState)
    } else {
      state
    }
  }

  case class State(tracks: Map[Position, Char], carts: Seq[Cart], rev: Long) {

    def nextState = {
      val ordered = cartsOrdered

      val newCarts = ordered.foldLeft((Seq[Cart](), Seq[Position](), carts)) {
        case ((movedCarts, crashes, notYetMovedCarts), cart) =>
          if (crashes contains cart.position) {
            // Someone hit us before we moved
            (movedCarts, crashes, notYetMovedCarts diff Seq(cart))
          } else {
            val newCart = cart.moveCart(tracks)
            val maybeCrashedWithMoved = movedCarts.find(_.position == newCart.position)
            val maybeCrashedWithNotYetMoved = notYetMovedCarts.find(_.position == newCart.position)

            (maybeCrashedWithNotYetMoved, maybeCrashedWithMoved) match {
              case (None, None) => (movedCarts :+ newCart, crashes, notYetMovedCarts diff Seq(cart))
              case (Some(notYetMovedCrashed), None) =>
                println("Crashed not yet moved!")
                (movedCarts, crashes :+ newCart.position, notYetMovedCarts diff Seq(notYetMovedCrashed, cart))
              case (None, Some(crashMoved)) =>
                println("Crashed moved")
                (movedCarts diff Seq(crashMoved), crashes :+ newCart.position, notYetMovedCarts diff Seq(cart))
              }
          }
      }

      State(tracks, newCarts._1, rev + 1)
    }

    def cartsOrdered = {
      carts.sortBy(_.position)
    }

    override def toString: String = {
      val lines = for {
        y <- 0 to 8
      } yield {
        val line = for {
          x <- 0 to 15
        } yield {
          val maybeCart = carts.find(_.position == (x, y)).map(_.directionAsChar)
          maybeCart.getOrElse(tracks.getOrElse((x, y), ' '))
        }

        line.mkString("")
      }

      val r = lines.mkString("\n")

      r + "\n" + carts
    }
  }

  val North = 0
  val East = 1
  val South = 2
  val West = 3
  //Seq(NORTH, EAST, SOUTH, WEST)

  sealed trait Turn
  object LEFT extends Turn
  object RIGHT extends Turn
  object STRAIGHT extends Turn

  val Left = 0
  val Straight = 1
  val Right = 2
//    Seq(LEFT, STRAIGHT, RIGHT)

  case class Cart(direction: Int, position: Position, onNextJunction: Int) {

    def moveCart(tracks: Map[Position, Char]): Cart = {
      val newPosition = direction match {
        case North => (position._1, position._2 - 1)
        case East => (position._1 + 1, position._2)
        case South => (position._1, position._2 + 1)
        case West => (position._1 - 1, position._2)
      }

      val newTrack = tracks(newPosition)

      try {
        Cart(newDirection(newTrack), newPosition, newOnNextJunction(newTrack))
      } catch {
        case e: Exception => throw new RuntimeException(s"On position $newPosition", e)
      }
    }

    def newDirection(track: Char) = {
      track match {
        case '|' =>
          if (direction != 0 && direction != 2) throw new RuntimeException(s"Direction $direction on track |")
          direction
        case '-' =>
          if (direction != 1 && direction != 3) throw new RuntimeException(s"Direction $direction on track -")
          direction
        case '\\' =>
          direction match {
            case East => South
            case North => West
            case South => East
            case West => North
          }
        case '/' =>
          direction match {
            case North => East
            case East => North
            case South => West
            case West => South
          }
        case '+' => turn
      }
    }

    def turn = {
      val newDir = direction + (onNextJunction - 1)
      (newDir + 4) % 4
    }

    def newOnNextJunction(newTrack: Char) = {
      if (newTrack == '+') {
        (onNextJunction + 1) % 3
      } else {
        onNextJunction
      }
    }

    def directionAsChar = direction match {
      case North => '^'
      case South => 'v'
      case East => '>'
      case West => '<'
    }

  }

  object Cart {

    def apply(direction: Char, position: Position, onNextJunction: Int) = {
      direction match {
        case '^' => new Cart(0, position, onNextJunction)
        case 'v' | 'V' => new Cart(2, position, onNextJunction)
        case '<' => new Cart(3, position, onNextJunction)
        case '>' => new Cart(1, position, onNextJunction)
      }
    }
  }

  def mapPosition[A](lines: Iterable[String], mapping: PartialFunction[(Position, Char), A]) = {
    lines.zipWithIndex.flatMap {
      case (line, lineIdx) =>
        line.zipWithIndex.collect {
          case (char, charIdx) if mapping.isDefinedAt((charIdx, lineIdx), char) =>
            val position = (charIdx, lineIdx)
            position -> mapping(position, char)
        }
    }.toMap
  }
}


