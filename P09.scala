/**
(**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */

// 1- My solution
/**
 * Similar solution to recusive P08 solution
 */
def pack(input: List[Any]): List[List[Any]] = {
  def comp(l: List[Any], lastElem: Any, lastList: List[Any]): List[List[Any]] = {
    l match {
      case Nil => List[List[Any]](lastList)
      case head :: tail if head == lastElem => comp(tail, head, head :: lastList)
      case head :: tail if lastList == Nil => comp(tail, head, head :: Nil)
      case head :: tail => lastList :: comp(tail, head, head :: Nil)
    }
  }
  comp(input, Nil, Nil)
}

// 2- Their solution
// span can be used on a list to split it in two (demarcated by a value)
def pack2[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span { _ == ls.head }
    if (next == Nil) List(packed)
    else packed :: pack2(next)
  }
}

// 3- Tests
def solutions:List[(List[Any] => List[List[Any]] )] = List(pack, pack2)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => List[List[Any]])=>
  assert (f(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) ==
    List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')),
    "Given example")
  assert(f(List(1,2,3)) == List(List(1),List(2),List(3)), "Simple example, no consecutive duplicates")
  assert(f(List(1,2,3,3)) == List(List(1),List(2),List(3,3)), "Simple example, 1 consecutive duplicate")
}
println("All tests passed!")