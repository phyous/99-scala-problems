// P82 (*) Cycle from a given node.
//     Write a method named findCycles to find closed paths (cycles) starting at
//     a given node in a graph.  The method should return all cycles.
//
//     scala> Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f")
//     res0: List[List[String]] = List(List(f, c, b, f), List(f, b, c, f))

abstract class GraphBase[T, U] {
  def findCycles(source: T): List[List[T]] = {
    val n = nodes(source)
    n.adj.map(edgeTarget(_, n).get.value).flatMap(findPaths(_, source)).map(source :: _).filter(_.lengthCompare(3) > 0)
  }
}
