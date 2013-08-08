// P81 (**) Path from one node to another one.
//     Write a method named findPaths to find an acyclic path from one node to
//     another in a graph.  The method should return all paths.
//
//     scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q")
//     res0: List[List[String]] = List(List(p, q), List(p, m, q))
//     
//     scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k")
//     res1: List[List[String]] = List()

abstract class GraphBase[T, U] {
  def findPaths(source: T, dest: T): List[List[T]] = {
    def findPathsR(curNode: Node, curPath: List[T]): List[List[T]] = {
      if (curNode.value == dest) List(curPath)
      else curNode.adj.map(edgeTarget(_, curNode).get).filter(n => !curPath.contains(n.value)).flatMap(n => findPathsR(n, n.value :: curPath))
    }
    findPathsR(nodes(source), List(source)).map(_.reverse)
  }
}
