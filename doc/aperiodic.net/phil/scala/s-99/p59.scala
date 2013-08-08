// P59 (**) Construct height-balanced binary trees.
//     In a height-balanced binary tree, the following property holds for every
//     node: The height of its left subtree and the height of its right subtree
//     are almost equal, which means their difference is not greater than one.
//    
//     Write a method Tree.hbalTrees to construct height-balanced binary trees
//     for a given height with a supplied value for the nodes.  The function
//     should generate all solutions.
//
//     scala> Tree.hbalTrees(3, "x")
//     res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...

object Tree {
  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case 1          => List(Node(value))
    case _ => {
      val fullHeight = hbalTrees(height - 1, value)
      val short = hbalTrees(height - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
      fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }
}
