// P62B (*) Collect the nodes at a given level in a list.
//      A node of a binary tree is at level N if the path from the root to the
//      node has length N-1.  The root node is at level 1.  Write a method
//      atLevel to collect all nodes at a given level in a list.
//
//      scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
//      res0: List[Char] = List(b, c)
//
//      Using atLevel it is easy to construct a method levelOrder which creates
//      the level-order sequence of the nodes.  However, there are more
//      efficient ways to do that.

sealed abstract class Tree[+T] {
  def atLevel(level: Int): List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def atLevel(level: Int): List[T] = level match {
    case n if n < 1 => Nil
    case 1          => List(value)
    case n          => left.atLevel(n - 1) ::: right.atLevel(n - 1)
  }
}

case object End extends Tree[Nothing] {
  def atLevel(level: Int) = Nil
}
