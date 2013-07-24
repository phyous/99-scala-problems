/**
Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */

// 1- My solution
def encode[A](input:List[A]):List[Any] = {
  def encodedChunck(l:List[A]) = if(l.length == 1) l.head else (l.length, l.head)

  if (input.isEmpty) List()
  else {
    val (head, next) = input.span { _ == input.head }
    val encoded = encodedChunck(head)
    if (next == Nil) List(encoded)
    else encoded :: encode(next)
  }
}

// 2- Their solution - pretty similar, except mapping at a higher level
// Their use of Either for type safety is neat, but it yields the following:
// List(Right((4,'a)), Left('b), Right((2,'c)), Right((2,'a)), Left('d), Right((4,'e)))
// SO I'm not sure if this meets requirements

// 3- Tests
def solutions:List[(List[Any] => List[Any] )] = List(encode)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => List[Any])=>
  assert (f(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
    List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)),
    "Given example")
  assert(f(List(1,2,3)) == List(1,2,3), "Simple example, no consecutive duplicates")
  assert(f(List(1,2,3,3)) == List(1,2,(2,3)), "Simple example, 1 consecutive duplicate")
  assert(f(List()) == List(), "Empty input list")
}
println("All tests passed!")