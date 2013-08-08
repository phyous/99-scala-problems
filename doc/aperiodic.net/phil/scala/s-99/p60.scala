// P60 (**) Construct height-balanced binary trees with a given number of nodes.
//     Consider a height-balanced binary tree of height H.  What is the maximum
//     number of nodes it can contain?  Clearly, MaxN = 2H - 1.  However, what
//     is the minimum number MinN?  This question is more difficult.  Try to
//     find a recursive statement and turn it into a function minHbalNodes that
//     takes a height and returns MinN.
//
//     scala> minHbalNodes(3)
//     res0: Int = 4
//
//     On the other hand, we might ask: what is the maximum height H a
//     height-balanced binary tree with N nodes can have?  Write a maxHbalHeight
//     function.
//
//     scala> maxHbalHeight(4)
//     res1: Int = 3
//
//     Now, we can attack the main problem: construct all the height-balanced
//     binary trees with a given nuber of nodes.
//
//     scala> Tree.hbalTreesWithNodes(4, "x")
//     res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
//
//     Find out how many height-balanced trees exist for N = 15.

sealed abstract class Tree[+T] {
  def nodeCount: Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def nodeCount: Int = left.nodeCount + right.nodeCount + 1
}

case object End extends Tree[Nothing] {
  def nodeCount: Int = 0
}

object Tree {
  def minHbalNodes(height: Int): Int = height match {
    case n if n < 1 => 0
    case 1          => 1
    case n          => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
  }
  def maxHbalNodes(height: Int): Int = 2 * height - 1
  def minHbalHeight(nodes: Int): Int =
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1
  def maxHbalHeight(nodes: Int): Int = 
    Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last
  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
    (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList
}
