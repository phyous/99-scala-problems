// P88 (**) Connected components.
//     Write a function that splits a graph into its connected components.
//
//     scala> Graph.fromString("[a-b, c]").splitGraph
//     res0: List[Graph[String,Unit]] = List([a-b], [c])

abstract class GraphBase[T, U] {
  case class Node(value: T) {
    // partners are all nodes either adjacent to this node or to which this
    // node is adjacent.
    def partners: List[Node] =
      edges.map(edgePartner(_, this)).remove(_.isEmpty).map(_.get).removeDuplicates
  }

  // If node N is a member of edge E, returns the other member of the edge.
  // This differs from edgeTarget in that if it is given an edge in a directed
  // graph and N is the target of that edge, it will still return the source
  // node for the edge.
  def edgePartner(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def splitGraph: List[GraphBase[T,U]] = {
    def nodes2Graph(nodes: List[Node]): GraphBase[T,U] = {
      val adjacentForm = nodes.map(n => (n.value, n.adj.map(e =>
        (edgeTarget(e, n).get.value, e.value))))
      this match {
        case _: Graph[_,_] => Graph.adjacentLabel(adjacentForm)
        case _: Digraph[_,_] => Digraph.adjacentLabel(adjacentForm)
      }
    }
    def findConnectedNodes(candidates: List[Node], soFar: List[Node]): List[Node] =
      candidates match {
        case Nil => soFar
        case n :: tail => {
          val newNodes = n.partners -- soFar - n
          findConnectedNodes(tail.union(newNodes), n :: soFar)
        }
      }
    def splitGraphR(unsplit: List[Node]): List[GraphBase[T,U]] = unsplit match {
      case Nil => Nil
      case n :: tail => {
        val connectedNodes = findConnectedNodes(List(n), Nil)
        nodes2Graph(connectedNodes) :: splitGraphR(unsplit -- connectedNodes)
      }
    }
    splitGraphR(nodes.values.toList)
  }
}

// Just to avoid repetition, we can now define Graph.edgeTarget in terms of
// edgePartner.
class Graph[T, U] extends GraphBase[T, U] {
  def edgeTarget(e: Edge, n: Node): Option[Node] = edgePartner(e, n)
}
