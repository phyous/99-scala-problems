// P80 (***) Conversions.
//     Write methods to generate the graph-term and adjacency-list forms from a
//     Graph.  Write another method to output the human-friendly form for a
//     graph.  Make it the toString method for Graph.  Write one more function
//     to create a graph of Chars and Ints from a human-friendly string.  Make
//     it implicitly convert from Strings to Graphs.
//
//     scala> Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
//     res0: (List[String], List[(String, String, Unit)]) = (List(d, k, h, c, f, g, b),List((h,g,()), (k,f,()), (f,b,()), (g,h,()), (f,c,()), (b,c,())))
//     
//     scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm
//     res1: List[(String, List[(String, Int)])] = List((m,List((q,7))), (p,List((m,5), (q,9))), (k,List()), (q,List()))

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    override def toString = value match {
      case () => n1.value + edgeSep + n2.value
      case v  => n1.value + edgeSep + n2.value + labelSep + v
    }
  }

  val edgeSep: String
  val labelSep: String = "/"
  
  override def toString = {
    val (edgeStrs, unlinkedNodes) =
      edges.foldLeft((Nil: List[String], nodes.values.toList))((r, e) => (e.toString :: r._1, r._2.filter((n) => n != e.n1 && n != e.n2)))
    "[" + (unlinkedNodes.map(_.value.toString) ::: edgeStrs).mkString(", ") + "]"
  }
  def toTermForm: (List[T], List[(T, T, U)]) =
    (nodes.keys.toList, edges.map((e) => (e.n1.value, e.n2.value, e.value)))
  def toAdjacentForm: List[(T, List[(T, U)])] =
    nodes.values.toList.map((n) => (n.value, n.adj.map((e) =>
      (edgeTarget(e, n).get.value, e.value))))
}

class Graph[T, U] extends GraphBase[T, U] {
  val edgeSep: String = "-"
}

class Digraph[T, U] extends GraphBase[T, U] {
  val edgeSep: String = ">"
}

abstract class GraphObjBase {
    val edgeSep: String
    val labelSep: String = "/"

    def fromStringBase[U, V](s: String)(mkGraph: (List[String], List[V]) => GraphClass[String, U], mkDigraph: (List[String], List[V]) => GraphClass[String, U])(parseEdge: String => V): GraphClass[String, U] = {
      assert(s(0) == '[')
      assert(s(s.length - 1) == ']')
      val tokens = s.substring(1, s.length - 1).split(", *").toList
      val nodes = tokens.flatMap(_.replaceAll("/.*", "").split("[->]")).removeDuplicates
      val edges = tokens.filter(_.matches(".*[->].*")).map(parseEdge)
      tokens.find(_.matches(".*>.*")) match {
        case None    => mkGraph(nodes, edges)
        case Some(_) => mkDigraph(nodes, edges)
      }
    }
    def fromString(s: String): GraphClass[String, Unit] =
      fromStringBase(s)(Graph.term[String], Digraph.term[String]) { t =>
        val split = t.split("[->]")
        (split(0), split(1))
      }
    def fromStringLabel(s: String): GraphClass[String, Int] = 
      fromStringBase(s)(Graph.termLabel[String, Int], Graph.termLabel[String, Int]) { t =>
        val split = t.split("[->]")
        val split2 = split(1).split("/")
        (split(0), split2(0), split2(1).toInt)
      }
}

object Graph extends GraphObjBase {
  val edgeSep: String = "-"
}

object Digraph extends GraphObjBase {
  val edgeSep: String = ">"
}
