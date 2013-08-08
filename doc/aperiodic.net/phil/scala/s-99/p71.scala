// P71 (*) Determine the internal path length of a tree.
//     We define the internal path length of a multiway tree as the total sum of
//     the path lengths from the root to all nodes of the tree.  By this
//     definition, the tree in the figure of problem P70 has an internal path
//     length of 9.  Write a method internalPathLength to return that sum.
//
//     scala> "afg^^c^bd^e^^^".internalPathLength
//     res0: Int = 9

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def internalPathLength: Int = 
    children.foldLeft(0)((r, c) => r + c.nodeCount + c.internalPathLength)
}
