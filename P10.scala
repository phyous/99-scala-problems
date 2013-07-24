/**
Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:

scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  */

// 1- My solution
// Take P09 and tack on a map
def encode[A](input:List[A]):List[(Int, A)] = {
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
  pack(input).map{ p =>
    (p.length, p.head)
  }
}

// P09 provided solution with slight modifications
// Can probably make more efficient by coding our own span and not needing to call "length" on list
def encode2[A](input:List[A]):List[(Int, A)] = {
  if (input.isEmpty) List()
  else {
    val (head, next) = input.span { _ == input.head }
    val encoded = (head.length, head.head)
    if (next == Nil) List(encoded)
    else encoded :: encode2(next)
  }
}

// 2- Their solution - Same as mine

// 3- Tests
def solutions:List[(List[Any] => List[(Int, Any)] )] = List(encode, encode2)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => List[(Int, Any)])=>
  assert (f(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
    List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)),
    "Given example")
  assert(f(List(1,2,3)) == List((1,1),(1,2),(1,3)), "Simple example, no consecutive duplicates")
  assert(f(List(1,2,3,3)) == List((1,1),(1,2),(2,3)), "Simple example, 1 consecutive duplicate")
}
println("All tests passed!")