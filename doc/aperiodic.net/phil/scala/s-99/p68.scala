// P68 (**) Preorder and inorder sequences of binary trees.
//     We consider binary trees with nodes that are identified by single
//     lower-case letters, as in the example of problem P67.
//
//     a) Write methods preorder and inorder that construct the preorder and
//        inorder sequence of a given binary tree, respectively.  The results
//        should be lists, e.g. List('a','b','d','e','c','f','g') for the
//        preorder sequence of the example in problem P67.
//
//     scala> Tree.string2Tree("a(b(d,e),c(,f(g,)))").preorder
//     res0: List[Char] = List(a, b, d, e, c, f, g)
//     
//     scala> Tree.string2Tree("a(b(d,e),c(,f(g,)))").inorder
//     res1: List[Char] = List(d, b, e, a, c, g, f)
//
//     b) If both the preorder sequence and the inorder sequence of the nodes of
//        a binary tree are given, then the tree is determined unambiguously.
//        Write a method preInTree that does the job.
//
//     scala> Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
//     res2: Node[Char] = a(b(d,e),c(,f(g,)))
//
//     What happens if the same character appears in more than one node?  Try,
//     for instance, Tree.preInTree(List('a', 'b', 'a'), List('b', 'a', 'a')).

sealed abstract class Tree[+T] {
  def preorder: List[T]
  def inorder: List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def preorder: List[T] = value :: left.preorder ::: right.preorder
  def inorder: List[T] = left.inorder ::: value :: right.inorder
}

case object End extends Tree[Nothing] {
  def preorder = Nil
  def inorder = Nil
}

object Tree {
  def preInTree[T](pre: List[T], in: List[T]): Tree[T] = pre match {
    case Nil       => End
    case v :: preTail => {
      val (leftIn, rightIn) = in.span(_ != v)
      Node(v, preInTree(preTail.take(leftIn.length), leftIn),
           preInTree(preTail.drop(leftIn.length), rightIn))
    }
  }
}
