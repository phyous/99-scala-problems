// P85 (**) Graph isomorphism.
//     Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection
//     f: N1 → N2 such that for any nodes X,Y of N1, X and Y are adjacent if and
//     only if f(X) and f(Y) are adjacent.
//
//     Write a method that determines whether two graphs are isomorphic.
//
//     scala> Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]"))
//     res0: Boolean = true

// This problem is in NP (it's one of the few for which it is unknown whether
// it's NP-complete or not), so it gets slow very quickly.  Essentially, we
// consider every mapping from the nodes of one graph into the other, which
// is O(n!).  There are some heuristics to prune the search tree
// (isValidMapping), but that can only go so far.
//
// For a more efficient algorithm (O(n^2 log n)), see "Practical Graph
// Isomorphism" by Brendan D. McKay of Vanderbilt University.
// http://cs.anu.edu.au/~bdm/nauty/PGI/

abstract class GraphBase[T, U] {
  def isIsomorphicTo[R,S](o: GraphBase[R,S]): Boolean = {
    // Build a lazy list so we only have to evaluate as much as necessary.
    def listMappings(tNodes: List[Node], oNodes: List[o.Node]) =
      tNodes.projection.flatMap(tn => oNodes.projection.map((tn, _)))
    // Used on partially-filled isomorphisms to weed out some early.
    def isValidMapping(iso: Map[Node,o.Node]): Boolean = 
      nodes.values forall {tn =>
        (!iso.contains(tn) ||
         tn.neighbors.filter(iso.contains).forall(tnn => iso(tn).neighbors.contains(iso(tnn))))
      }
    def isValidCompleteMapping(iso: Map[Node,o.Node]): Boolean = 
      nodes.values forall {tn =>
        Set(tn.neighbors.map(iso.apply): _*) == Set(iso(tn).neighbors: _*)
      }
    def isIsomorphicToR(tNodes: List[Node], oNodes: List[o.Node], iso: Map[Node,o.Node]): Boolean =
      if (tNodes == Nil) isValidCompleteMapping(iso)
      else listMappings(tNodes, oNodes).filter(p => isValidMapping(iso + p)) exists {p =>
        isIsomorphicToR(tNodes - p._1, oNodes - p._2, iso + p)
      }
    isIsomorphicToR(nodes.values.toList, o.nodes.values.toList, Map())
  }
}
