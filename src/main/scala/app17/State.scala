package app17

case class State(clay: Map[Pos, Clay], stateWater: Map[Pos, Water], rev: Int) {

  val minY = clay.keySet.map(_.y).min
  val maxY = clay.keySet.map(_.y).max

  val maxWaterY = stateWater.keys.map(_.y).max + 1

  def flow(): State = {
    println(s"Watter bottom: $maxWaterY. Max Y: $maxY")

    val nextState = State(clay, nextWater(stateWater), rev + 1)
    if (nextState.stateWater == this.stateWater) {
      this
    } else {
      nextState.flow()
    }
  }

  def filterStreamBottoms(water: Map[Pos, Water]): Map[Pos, Water] = {
    water.filter {
      case (p, w) =>
        val left = water.getOrElse(p.left, fieldType(p.left))
        val right = water.getOrElse(p.right, fieldType(p.right))
        val below = water.getOrElse(p.below, fieldType(p.below))
        (w, left, right, below) match {
          case (FlowingWater, _, _, FlowingWater) => false
          case (FlowingWater, FlowingWater, FlowingWater, b) if !b.isPermeable => false
          case (FlowingWater, _: Clay, FlowingWater, b) if !b.isPermeable => false
          case (FlowingWater, FlowingWater, _: Clay, b) if !b.isPermeable => false
          case (StableWater, _, _, _) => false
          case _ => true
        }
    }
  }

  def nextWater(water: Map[Pos, Water]): Map[Pos, Water] = {
    val streamBottoms = filterStreamBottoms(water)
    val newWater = water ++ streamBottoms.keys.flatMap(b => spillWater(b, water))

    if (newWater == water) {
      water
    } else {
      nextWater(newWater)
    }
  }

  def spillWater(waterPos: Pos, currentWaterState: Map[Pos, Water]): Map[Pos, Water] = {
    val bucketWalls = findWallsOnCurrentLevel(waterPos)

    if (fieldType(waterPos.below).isPermeable) {
      val flow = waterPos.below
      if (flow.y <= maxY && !currentWaterState.keySet.contains(flow)) {
        Map(flow -> FlowingWater) ++ spillWater(flow, currentWaterState + (flow -> FlowingWater))
      } else {
        currentWaterState.filter(_._1 == waterPos)
      }
    } else {
      bucketWalls match {

        case (Some(left), Some(right)) =>
          (for {
            x <- left.x + 1 to right.x - 1
          } yield Pos(x, waterPos.y) -> StableWater).toMap

        case (Some(left), None) =>
          spillToRight(Pos(left.x + 1, left.y), currentWaterState)

        case (None, Some(right)) => {
          spillToLeft(Pos(right.x - 1, right.y), currentWaterState)
        }

        case (None, None) => spillToLeft(waterPos.left, currentWaterState) ++ spillToRight(waterPos.right, currentWaterState)
      }
    }
  }

  private def spillToRight(waterPos: Pos, currentWaterState: Map[Pos, Water]): Map[Pos, Water] = {
    if (fieldType(waterPos.below).isPermeable) {
      if (waterPos.below.y <= maxY) {
        Map(waterPos -> FlowingWater) ++ spillWater(waterPos.below, currentWaterState)
      } else {
        Map(waterPos -> FlowingWater)
      }
    } else {
      val spilled = for {
        x <- waterPos.x to getRightWater(waterPos).x
      } yield {
        Pos(x, waterPos.y) -> FlowingWater
      }

      spilled.toMap
    }
  }

  private def spillToLeft(waterPos: Pos, currentWaterState: Map[Pos, Water]): Map[Pos, Water] = {
    if (fieldType(waterPos.below).isPermeable) {
      if (waterPos.below.y <= maxY) {
        Map(waterPos -> FlowingWater) ++ spillWater(waterPos.below, currentWaterState)
      } else {
        Map(waterPos -> FlowingWater)
      }
    } else {
      val spilled = for {
        x <- getLeftWater(waterPos).x to waterPos.x
      } yield {
        Pos(x, waterPos.y) -> FlowingWater
      }

      spilled.toMap
    }
  }

  def getRightWater(pos: Pos) = {
    val aboveClay = Iterator.iterate(pos)(_.right).takeWhile(p => !fieldType(p.below).isPermeable && fieldType(p.right).isPermeable).toSeq.last
    if (fieldType(aboveClay.right).isPermeable) {
      aboveClay.right
    } else {
      aboveClay
    }

  }

  def getLeftWater(pos: Pos) = {
    val aboveClay = Iterator.iterate(pos)(_.left).takeWhile(p => !fieldType(p.below).isPermeable && fieldType(p.left).isPermeable).toSeq.last
    if (fieldType(aboveClay.left).isPermeable) {
      aboveClay.left
    } else {
      aboveClay
    }
  }

  def findWallsOnCurrentLevel(waterPos: Pos) = {
    val wallRight = findWallInOneDirection(waterPos, pos => pos.right)
    val wallLeft = findWallInOneDirection(waterPos, pos => pos.left)

    (wallLeft, wallRight)
  }

  def findWallInOneDirection(currentPos: Pos, nextPos: Pos => Pos): Option[Pos] = {
    if (fieldType(currentPos.below).isPermeable) {
      None
    } else if (!fieldType(nextPos(currentPos)).isPermeable) {
      Some(nextPos(currentPos))
    } else {
      findWallInOneDirection(nextPos(currentPos), nextPos)
    }
  }

  def fieldType(pos: Pos) = {
    clay.getOrElse(pos ,stateWater.getOrElse(pos, Sand(pos)))
  }

  override def toString() = toString(false)

  def toString(withLineNumbers: Boolean) = {
    val minX = stateWater.keySet.map(_.x).min
    val maxX = stateWater.keySet.map(_.x).max

    val lines = for {
      y <- 0 to maxWaterY
    } yield {
      val line = for {
        x <- minX - 1 to maxX + 1
      } yield {
        val c = stateWater.get(Pos(x, y)).map {
          case _: FlowingWater.type => "|"
          case _: StableWater.type => "~"
        }
          .getOrElse(clay.get(Pos(x, y)).map(_ => "#").getOrElse(" "))

        c
      }

      val prefix = if (withLineNumbers) {
        s"$y\t "
      } else {
        ""
      }

      prefix + line.mkString("")
    }

    lines.mkString("\n")
  }
}