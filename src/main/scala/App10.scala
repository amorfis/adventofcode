import scala.collection.immutable.Stream
import scala.io.Source

object App10 {

  val ScreenWidth = 200
  val ScreenHeight = 80

  case class SkyPoint(x: Int, y: Int, velocity: (Int, Int))
  case class ScreenPoint(x: Int, y: Int)

  case class State(points: Seq[SkyPoint]) {

    def spans = {
      val maxX = points.map(_.x).max
      val minX = points.map(_.x).min

      val maxY = points.map(_.y).max
      val minY = points.map(_.y).min

      val xSpan = maxX - minX
      val ySpan = maxY - minY

      (xSpan, ySpan)
    }

    def drawWithoutScaling(width: Int, height: Int) = {
      val maxX = points.map(_.x).max
      val minX = points.map(_.x).min

      val maxY = points.map(_.y).max
      val minY = points.map(_.y).min

      println(s"X: $minX, $maxX, Y: $minY, $maxY")

      val xSpan = maxX - minX
      val ySpan = maxY - minY

      def mapPosition(x: Int, y: Int) = {
        (x - minX, y - minY)
      }

      val screenPoints = points.map { p =>
        val (sx, sy) = mapPosition(p.x, p.y)
        ScreenPoint(sx.toInt, sy.toInt)
      }

      val screen = for {
        cy <- 1 to height
        cx <- 1 to width
      } yield {
        val pointsHere = screenPoints.count(_ == ScreenPoint(cx - 1, cy - 1))
        pointsHere match {
          case 0 => "."
          case i => 1.toString
        }
      }

      val screenLines = screen.grouped(width).toSeq
      screenLines.foreach(line => println(line.mkString("")))
    }

    def draw() = {
      val maxX = points.map(_.x).max
      val minX = points.map(_.x).min

      val maxY = points.map(_.y).max
      val minY = points.map(_.y).min

      println(s"X: $minX, $maxX, Y: $minY, $maxY")

      val xSpan = maxX - minX
      val ySpan = maxY - minY

      val xFactor = ScreenWidth.toDouble / xSpan
      val yFactor = ScreenHeight.toDouble / ySpan

      def mapPosition(x: Int, y: Int) = {
        (x * xFactor - minX * xFactor, y * yFactor - minY * yFactor)
      }

      val screenPoints = points.map { p =>
        val (sx, sy) = mapPosition(p.x, p.y)
        ScreenPoint(sx.toInt, sy.toInt)
      }

      val screen = for {
        cy <- 1 to ScreenHeight
        cx <- 1 to ScreenWidth
      } yield {
        val pointsHere = screenPoints.count(_ == ScreenPoint(cx, cy))
        pointsHere match {
          case 0 => "."
          case i => 1.toString
        }
      }

      val screenLines = screen.grouped(ScreenWidth).toSeq
      screenLines.foreach(line => println(line.mkString("")))
    }

    def nextState = {
      moveNStates(1)
    }

    def moveNStates(n: Int) = {
      val newPoints = points.map { p =>
        SkyPoint(p.x + n * p.velocity._1, p.y + n * p.velocity._2, p.velocity)
      }

      State(newPoints)
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input10.txt").getLines()

    val skyPoints = lines.map { line =>
      val r = """position=<(.*), (.*)> velocity=<(.*), (.*)>""".r
      //position=<-42162, -42124> velocity=< 4,  4>
      line match {
        case r(x, y, vx, vy) => SkyPoint(x.trim.toInt, y.trim.toInt, (vx.trim.toInt, vy.trim.toInt))
      }
    }.toSeq

    val Initial = 10550
    var state = State(skyPoints).moveNStates(Initial)

    val Step = 1

    var minXSpan = (10000, 0)
    var minYSpan = (10000, 0)

//    for {
//      i <- 1 to 50
//    } {
//      state = state.moveNStates(Step)
//      println(s"State ${i * Step + Initial}")
//      state.drawWithoutScaling(ScreenWidth, ScreenHeight)
//
//      val (xs, ys) = state.spans
//
//      if (xs < minXSpan._1) minXSpan = (xs, i * Step + Initial)
//      if (ys < minYSpan._1) minYSpan = (ys, i * Step + Initial)
//    }

    println(s"Min X span: $minXSpan")
    println(s"Min Y span: $minYSpan")

//    val s = State(skyPoints.toSeq).moveNStates(10560)
//    s.draw()

    val fs = State(skyPoints).moveNStates(10577)
    fs.drawWithoutScaling(100, 10)
  }

  def rollState(state: State, n: Int): State = {
    if (n == 0) {
      state
    } else {
      rollState(state, n - 1)
    }
  }

}


