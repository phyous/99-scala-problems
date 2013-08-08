// P58 (**) Generate-and-test paradigm.
//     Apply the generate-and-test paradigm to construct all symmetric,
//     completely balanced binary trees with a given number of nodes.
//
//     scala> Tree.symmetricBalancedTrees(5, "x")
//     res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))

object Tree {
  def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] =
    cBalanced(nodes, value).filter(_.isSymmetric)
}
