// P89 (**) Bipartite graphs.
//     Write a function that determines whether a given graph is bipartite.
//
//     scala> Digraph.fromString("[a>b, c>a, d>b]").isBipartite
//     res0: Boolean = true
//     
//     scala> Graph.fromString("[a-b, b-c, c-a]").isBipartite
//     res1: Boolean = false
//     
//     scala> Graph.fromString("[a-b, b-c, d]").isBipartite
//     res2: Boolean = true
//     
//     scala> Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite
//     res3: Boolean = false

abstract class GraphBase[T, U] {
  def isBipartiteInternal = {
    def isBipartiteR(evenToCheck: List[Node], oddToCheck: List[Node], evenSeen: Set[Node], oddSeen: Set[Node]): Boolean =
      (evenToCheck, oddToCheck) match {
        case (Nil, Nil) => true
        case (e :: eTail, odd) =>
          e.partners.forall(!evenSeen(_)) && isBipartiteR(eTail, odd.union(e.partners.remove(oddSeen(_))), evenSeen + e, oddSeen ++ e.partners)
        case (Nil, o :: oTail) =>
          o.partners.forall(!oddSeen(_)) && isBipartiteR(o.partners.remove(oddSeen(_)), oTail, evenSeen ++ o.partners, oddSeen + o)
      }
    isBipartiteR(List(nodes.values.next), Nil, Set(), Set())
  }
  def isBipartite: Boolean = {
    nodes.isEmpty || splitGraph.forall(_.isBipartiteInternal)
  }
}

