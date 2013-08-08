// P70C (*) Count the nodes of a multiway tree.
//      Write a method nodeCount which counts the nodes of a given multiway
//      tree.
//
//      scala> MTree('a', List(MTree('f'))).nodeCount
//      res0: Int = 2

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def nodeCount: Int = children.foldLeft(1)(_ + _.nodeCount)
}
