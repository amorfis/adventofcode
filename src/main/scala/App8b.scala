import scala.io.Source

object App8b {

  case class Node(children: Seq[Node], metadata: Seq[Int], numbersCount: Int) {

    def nodeValue: Int = {
      if (children.isEmpty) {
        metadata.sum
      } else {
        metadata.map(m => getChildOrNone(m - 1)).collect {
          case Some(c) => c
        }.map(_.nodeValue).sum
      }
    }

    def getChildOrNone(idx: Int) =
      if (children.size > idx) {
        Some(children(idx))
      } else {
        None
      }
  }

  def main(args: Array[String]): Unit = {

    val line = Source.fromResource("input8.txt").getLines().mkString
    val numbers = line.split("\\s").filter(_.nonEmpty).map(_.toInt)

    val testSub = Seq(2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2)
    //                A----------------------------------
    //                    B----------- C-----------
    //                                     D-----
    val bbb = getNode(testSub)
    println(bbb)
    println(bbb.nodeValue)

//    val testTree = getNode(Seq(1,3,1,1,0,1,99,2,1,1,2))
    //                         A---------------------
    //                             B-----------
    //                                 C-----
//    println(testTree)

    val treeRoot = getNode(numbers)

    println(treeRoot)
    println(sumMetadata(Seq(treeRoot)))
    println(treeRoot.nodeValue)
  }

  def sumMetadata(nodes: Seq[Node]): Int = {
    val topNodesMetadataSum = nodes.map(_.metadata.sum).sum
    val childrenMetadataSum = nodes.map(n => sumMetadata(n.children)).sum

    topNodesMetadataSum + childrenMetadataSum
  }

  def getNode(numbers: Seq[Int]): Node = {
    val subnodesCount = numbers.head
    val metadataCount = numbers.tail.head

    val subnodes = (0 until subnodesCount).foldLeft((Seq[Node](), numbers))((tuple, _) => getNextSubnodes(tuple))._1

    val subnodesLength = subnodes.map(_.numbersCount).sum
    val metadata = numbers.slice(2 + subnodesLength, 2 + subnodesLength + metadataCount)

    Node(subnodes, metadata, 2 + subnodesLength + metadataCount)
  }

  def getNextSubnodes(foldTuple: (Seq[Node], Seq[Int])) = {
    val subs = foldTuple match {
      case (nodes, numbersLeft) =>
        val node = getNode(numbersLeft.drop(2))
        (nodes :+ node, numbersLeft.drop(node.numbersCount))
    }

    subs
  }
}


