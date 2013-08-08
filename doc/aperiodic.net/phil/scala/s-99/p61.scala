// P61 (*) Count the leaves of a binary tree.
//     A leaf is a node with no successors.  Write a method leafCount to count
//     them.
//
//     scala> Node('x', Node('x'), End).leafCount
//     res0: Int = 1

sealed abstract class Tree[+T] {
  def leafCount: Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case _          => left.leafCount + right.leafCount
  }
}

case object End extends Tree[Nothing] {
  def leafCount: Int = 0
}
