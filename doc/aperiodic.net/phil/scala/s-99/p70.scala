// P70 (**) Tree construction from a node string.
//     We suppose that the nodes of a multiway tree contain single characters.
//     In the depth-first order sequence of its nodes, a special character ^ has
//     been inserted whenever, during the tree traversal, the move is a
//     backtrack to the previous level.
//
//     By this rule, the tree in the figure opposite is represented as:
//     afg^^c^bd^e^^^
//
//     Define the syntax of the string and write a function string2MTree to
//     construct an MTree from a String.  Make the function an implicit
//     conversion from String.  Write the reverse function, and make it the
//     toString method of MTree.
//
//     scala> MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
//     res0: String = afg^^c^bd^e^^^

case class MTree[+T](value: T, children: List[MTree[T]]) {
  override def toString = value.toString + children.map(_.toString + "^").mkString("")
}

object MTree {
  implicit def string2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
  }
}
