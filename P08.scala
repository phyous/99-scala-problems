/**
Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  */

/*
 Tail recursive solution
 More efficient on space since we save stack allocations
  See "What is tail call optimizaition"
  http://stackoverflow.com/questions/310974/what-is-tail-call-optimization
 Not super efficient in time since we need to reverse at the end
 */
def compress1(input: List[Any]): List[Any] = {
  def comp(current: List[Any], l: List[Any], last: Any): List[Any] = {
    current match {
      case Nil => l
      case head :: tail if head == last => comp(tail, l, head)
      case head :: tail => comp(tail, head :: l, head)
    }
  }
  comp(input, Nil, Nil).reverse
}

/*
 Normal recursive method
 */
def compress2(input: List[Any]): List[Any] = {
  def comp(l: List[Any], last: Any): List[Any] = {
    l match {
      case Nil => Nil
      case head :: tail if head == last => comp(tail, head)
      case head :: tail => head :: comp(tail, head)
    }
  }
  comp(input, Nil)
}

def solutions:List[(List[Any] => List[Any] )] = List(compress1, compress2)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => List[Any])=>
  assert (f(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) ==
    List('a', 'b', 'c', 'a', 'd', 'e'),
    "Given example")
  assert(f(List(1,2,3)) == List(1,2,3), "Simple example, no consecutive duplicates")
  assert(f(List(1,2,3,3)) == List(1,2,3), "Simple example, 1 consecutive duplicate")
}
println("All tests passed!")