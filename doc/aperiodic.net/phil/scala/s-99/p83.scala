// P83 (**) Construct all spanning trees.
//     Write a method spanningTrees to construct all spanning trees of a given
//     graph.  With this method, find out how many spanning trees there are for
//     the graph depicted to the right.  The data of this example graph can be
//     found below.  When you have a correct solution for the spanningTrees
//     method, use it to define two other useful methods: isTree and
//     isConnected.  Both are five-minute tasks!
//    
//     Graph:
//     Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
//                List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
//                     ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
//                     ('e', 'h'), ('f', 'g'), ('g', 'h')))
//
//     scala> Graph.fromString("[a-b, b-c, a-c]").spanningTrees
//     res0: List[Graph[String,Unit]] = List([a-b, b-c], [a-c, b-c], [a-b, a-c])

// Only undirected graphs have spanning trees.

class Graph[T, U] extends GraphBase[T, U] {
  // edgeConnectsToGraph is needed for P84, so it's not an internal function.
  def edgeConnectsToGraph[T,U](e: Edge, nodes: List[Node]): Boolean =
    !(nodes.contains(e.n1) == nodes.contains(e.n2))  // xor
  def spanningTrees = {
    def spanningTreesR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): List[Graph[T,U]] = {
      if (graphNodes == Nil) List(Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple)))
      else if (graphEdges == Nil) Nil
      else graphEdges.filter(edgeConnectsToGraph(_, graphNodes)) flatMap { ge =>
        spanningTreesR(graphEdges.remove(_ == ge),
                       graphNodes.filter(edgeTarget(ge, _) == None),
                       ge :: treeEdges)
                                                                        }
    }
    spanningTreesR(edges, nodes.values.toList.tail, Nil).removeDuplicates
  }
  def isTree: Boolean = spanningTrees.lengthCompare(1) == 0
  def isConnected: Boolean = spanningTrees.lengthCompare(0) > 0
}
