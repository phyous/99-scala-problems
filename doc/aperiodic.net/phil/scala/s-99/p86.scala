// P86 (**) Node degree and graph coloration.
//     a) Write a method Node.degree that determines the degree of a given node.
//
//     scala> Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("a").degree
//     res0: Int = 3
//
//     b) Write a method that lists all nodes of a graph sorted according to
//        decreasing degree.
//
//     scala> Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree
//     res1: List[Graph[String,Unit]#Node] = List(Node(a), Node(c), Node(b), Node(d))
//
//     c) Use Welsh-Powell's algorithm to paint the nodes of a graph in such a
//        way that adjacent nodes have different colors.  Make a method
//        colorNodes that returns a list of tuples, each of which contains a
//        node and an integer representing its color.
//
//     scala> Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes
//     res2: List[(Graph[String,Unit]#Node,Int)] = List((Node(a),1), (Node(b),2), (Node(c), 3), (Node(d), 2))

abstract class GraphBase[T, U] {
  case class Node(value: T) {
    def degree: Int = edges.length
  }

  def nodesByDegree: List[Node] = nodes.values.toList.sort(_.degree > _.degree)
  def colorNodes: List[(Node,Int)] = {
    import collection.immutable.Set
    def applyColor(color: Int, uncolored: List[Node], colored: List[(Node,Int)], adjacentNodes: Set[Node]): List[(Node,Int)] =
      uncolored match {
        case Nil => colored
        case n :: tail => {
          val newAdjacent = adjacentNodes ++ n.neighbors
          applyColor(color, tail.dropWhile(newAdjacent.apply), (n, color) :: colored, newAdjacent)
        }
      }
    def colorNodesR(color: Int, uncolored: List[Node], colored: List[(Node,Int)]): List[(Node,Int)] =
      if (uncolored == Nil) colored
      else {
        val newColored = applyColor(color, uncolored, colored, Set())
        colorNodesR(color + 1, uncolored -- newColored.map(_._1), newColored)
      }
    colorNodesR(1, nodesByDegree, Nil)
  }
}
