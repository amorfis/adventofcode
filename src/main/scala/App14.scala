import scala.collection.mutable.ArrayBuffer

object App14 {

  def main(args: Array[String]): Unit = {

    val testRecipesCount = 681901

    val finalState = rollUntilRecipesCount(State(), testRecipesCount + 10)

    val final10 = finalState.recipes.slice(testRecipesCount, testRecipesCount + 10)
    println(final10)
  }

  def rollUntilRecipesCount(state: State, count: Int): State = {
    if (state.recipes.size % 10000 == 0) {
      println(s"Current recipes count: ${state.recipes.size}")
    }

    if (state.recipes.size < count) {
      rollUntilRecipesCount(state.next(), count)
    } else {
      state
    }
  }

  case class State(recipes: Seq[Int], elfOneCurrent: Int, elfTwoCurrent: Int) {
    val generateNewRecipes = recipes ++ toSeqOfDigits(recipes(elfOneCurrent) + recipes(elfTwoCurrent))

    def next() = {
      val newR = generateNewRecipes
      val newElfOne = (elfOneCurrent + 1 + recipes(elfOneCurrent)) % newR.size
      val newElfTwo = (elfTwoCurrent + 1 + recipes(elfTwoCurrent)) % newR.size

      State(newR, newElfOne, newElfTwo)
    }

    override def toString: String = {
      def inParen(idx: Int) = {
        if (idx == elfOneCurrent) {
          "(" + recipes(idx) + ")"
        } else {
          "[" + recipes(idx) + "]"
        }
      }

      val minElf = Seq(elfOneCurrent, elfTwoCurrent).min
      val maxElf = Seq(elfOneCurrent, elfTwoCurrent).max

      recipes.take(minElf).mkString(" ") +
        inParen(minElf) +
        recipes.slice(minElf + 1, maxElf).mkString(" ") +
        inParen(maxElf) +
        recipes.drop(maxElf + 1).mkString(" ")

    }
  }

  def toSeqOfDigits(number: Int) = {
    number.toString.toCharArray.map(_.toString.toInt)
  }

  object State {

    def apply() = {
      new State(ArrayBuffer(3, 7), 0, 1)
    }
  }
}


