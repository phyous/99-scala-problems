package graph {

  abstract class GraphBase[T, U] {
    case class Edge(n1: Node, n2: Node, value: U) {
      def toTuple = (n1.value, n2.value, value)
      override def toString = value match {
        case () => n1.value + edgeSep + n2.value
        case v  => n1.value + edgeSep + n2.value + labelSep + v
      }
    }
    case class Node(value: T) {
      var adj: List[Edge] = Nil
      // neighbors are all nodes adjacent to this node.
      def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
      // partners are all nodes either adjacent to this node or to which this
      // node is adjacent.
      def partners: List[Node] =
        edges.map(edgePartner(_, this)).remove(_.isEmpty).map(_.get).removeDuplicates
      def degree: Int = adj.length
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

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil

    val edgeSep: String
    val labelSep: String = "/"

    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

    // If node N is a member of edge E, returns the other member of the edge.
    // This differs from edgeTarget in that if it is given an edge in a directed
    // graph and N is the target of that edge, it will still return the source
    // node for the edge.
    def edgePartner(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else if (e.n2 == n) Some(e.n1)
      else None

    override def equals(o: Any) = o match {
      case g: GraphBase[_,_] => (nodes.keys.toList -- g.nodes.keys.toList == Nil &&
                                 edges.map(_.toTuple) -- g.edges.map(_.toTuple) == Nil)
      case _ => false
    }

    // P80
    override def toString = {
      val (edgeStrs, unlinkedNodes) =
        edges.foldLeft((Nil: List[String], nodes.values.toList))((r, e) => (e.toString :: r._1, r._2.filter((n) => n != e.n1 && n != e.n2)))
      "[" + (unlinkedNodes.map(_.value.toString) ::: edgeStrs).mkString(", ") + "]"
    }

    def addNode(value: T) = {
      val n = new Node(value)
      nodes = Map(value -> n) ++ nodes
      n
    }

    // P80
    def toTermForm: (List[T], List[(T, T, U)]) = 
      (nodes.keys.toList, edges.map(_.toTuple))
    def toAdjacentForm: List[(T, List[(T, U)])] =
      nodes.values.toList.map((n) => (n.value, n.adj.map(e =>
        (edgeTarget(e, n).get.value, e.value))))

    // P81
    def findPaths(source: T, dest: T): List[List[T]] = {
      def findPathsR(curNode: Node, curPath: List[T]): List[List[T]] = {
        if (curNode.value == dest) List(curPath)
        else curNode.neighbors.filter(n => !curPath.contains(n.value)).flatMap(n => findPathsR(n, n.value :: curPath))
      }
      findPathsR(nodes(source), List(source)).map(_.reverse)
    }

    // P82
    def findCycles(source: T): List[List[T]] = {
      val n = nodes(source)
      n.neighbors.map(_.value).flatMap(findPaths(_, source)).map(source :: _).filter(_.lengthCompare(3) > 0)
    }

    // P85
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

    // P86
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

    // P87
    def nodesByDepthFrom(start: T): List[T] = 
      nodes(start).nodesByDepth(Set()).map(_.value)

    // P88
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

    // P89
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

  class Graph[T, U] extends GraphBase[T, U] {
    val edgeSep: String = "-"

    override def equals(o: Any) = o match {
      case g: Graph[_,_] => super.equals(g)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] = edgePartner(e, n)

    def addEdge(n1: T, n2: T, value: U) = {
      val e = new Edge(nodes(n1), nodes(n2), value)
      edges = e :: edges
      nodes(n1).adj = e :: nodes(n1).adj
      nodes(n2).adj = e :: nodes(n2).adj
    }

    // P83
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

    // P84
    def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T,U] = {
      def minimalSpanningTreeR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): Graph[T,U] =
        if (graphNodes == Nil) Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple))
        else {
          val nextEdge = graphEdges.filter(edgeConnectsToGraph(_, graphNodes)).reduceLeft((r, e) => if (r.value < e.value) r else e)
          minimalSpanningTreeR(graphEdges - nextEdge, 
                               graphNodes.filter(edgeTarget(nextEdge, _) == None),
                               nextEdge :: treeEdges)
        }
      minimalSpanningTreeR(edges, nodes.values.toList.tail, Nil)
    }
  }
  
  class Digraph[T, U] extends GraphBase[T, U] {
    val edgeSep: String = ">"

    override def equals(o: Any) = o match {
      case g: Digraph[_,_] => super.equals(g)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else None

    def addArc(source: T, dest: T, value: U) = {
      val e = new Edge(nodes(source), nodes(dest), value)
      edges = e :: edges
      nodes(source).adj = e :: nodes(source).adj
    }
  }
  
  abstract class GraphObjBase {
    type GraphClass[T, U]
    val edgeSep: String
    val labelSep: String = "/"

    def addLabel[T](edges: List[(T, T)]) =
      edges.map(v => (v._1, v._2, ()))
    def term[T](nodes: List[T], edges: List[(T,T)]) =
      termLabel(nodes, addLabel(edges))
    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
    def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
      nodes.map(a => (a._1, a._2.map((_, ()))))
    def adjacent[T](nodes: List[(T, List[T])]) =
      adjacentLabel(addAdjacentLabel(nodes))
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]

    // P80
    def fromStringBase[U, V](s: String)(mkGraph: (List[String], List[V]) => GraphClass[String, U])(parseEdge: String => V): GraphClass[String, U] = {
      assert(s(0) == '[')
      assert(s(s.length - 1) == ']')
      val tokens = s.substring(1, s.length - 1).split(", *").toList
      val nodes = tokens.flatMap(_.replaceAll(labelSep + ".*", "").split(edgeSep)).removeDuplicates
      val edges = tokens.filter(_.matches(".*" + edgeSep + ".*")).map(parseEdge)
      mkGraph(nodes, edges)
    }
    def fromString(s: String): GraphClass[String, Unit] =
      fromStringBase(s)(term[String]) { t =>
        val split = t.split(edgeSep)
        (split(0), split(1))
      }
    def fromStringLabel(s: String): GraphClass[String, Int] = 
      fromStringBase(s)(termLabel[String, Int]) { t =>
        val split = t.split(edgeSep)
        val split2 = split(1).split(labelSep)
        (split(0), split2(0), split2(1).toInt)
      }
  }
  
  object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]
    val edgeSep: String = "-"

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
      val g = new Graph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addEdge(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
      val g = new Graph[T, U]
      for ((v, _) <- nodes) g.addNode(v)
      for ((n1, a) <- nodes; (n2, l) <- a) {
        if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
          g.addEdge(n1, n2, l)
      }
      g
    }
  }
  
  object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]
    val edgeSep: String = ">"

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
      val g = new Digraph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addArc(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
      val g = new Digraph[T, U]
      for ((n, a) <- nodes) g.addNode(n)
      for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
      g
    }
  }

}
