// P72 (*) Construct the postorder sequence of the tree nodes.
//     Write a method postorder which constructs the postorder sequence of the
//     nodes of a multiway tree.  The result should be a List.
//
//     scala> "afg^^c^bd^e^^^".postorder
//     res0: List[Char] = List(g, f, c, d, e, b, a)

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def postorder: List[T] = children.flatMap(_.postorder) ::: List(value)
}
