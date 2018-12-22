package app18

trait Field {
  def isTree: Boolean
  def isLumberyard: Boolean
}

object Space extends Field {
  val isTree= false
  val isLumberyard = false
}

object Tree extends Field {
  val isTree = true
  val isLumberyard = false
}

object Lumberyard extends Field {
  val isTree = false
  val isLumberyard = true
}
