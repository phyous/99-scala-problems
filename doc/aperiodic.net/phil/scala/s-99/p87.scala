// P87 (**) Depth-first order graph traversal.
//     Write a method that generates a depth-first order graph traversal
//     sequence.  The starting point should be specified, and the output should
//     be a list of nodes that are reachable from this starting point (in
//     depth-first order).
//
//     scala> Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom("d")
//     res0: List[String] = List(c, b, a, d)

// Node.nodesByDepth is a little inefficient.  With immutable Lists, it ends up
// rebuilding the entire list every time it adds a node.  It would be more
// efficient to build the list backwards (adding new elements to the beginning)
// and then reverse it in nodesByDepthFrom.
//
// Similarly, nodesByDepthR isn't tail recursive.  If a node has more neighbors
// than there are stack frames, this will be a problem.

abstract class GraphBase[T, U] {
  case class Node(value: T) {
    def nodesByDepth(seen: Set[Node]): List[Node] = {
      def nodesByDepthR(neighbors: List[Node], s: Set[Node]): List[Node] = 
        neighbors match {
          case Nil => Nil
          case n :: tail if s(n) => nodesByDepthR(tail, s)
          case n :: tail => {
            val subnodes = n.nodesByDepth(s)
            subnodes ::: nodesByDepthR(tail, s ++ subnodes)
          }
        }
      nodesByDepthR(neighbors, seen + this) ::: List(this)
    }
  }

  def nodesByDepthFrom(start: T): List[T] = 
    nodes(start).nodesByDepth(Set()).map(_.value)
}
