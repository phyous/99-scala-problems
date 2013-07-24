/**
Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:

scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  */

// 1- My solution
def decode[A](input:List[(Int, A)]):List[A] = {
  input.flatMap { e => List.fill(e._1)(e._2) }
}

// 2- Their solution
// They are doing the same thing, except using List.make which is now deprecated in favor of List.fill

// 3- Tests
def solutions:List[(List[(Int, Any)] => List[Any] )] = List(decode)

println("Starting tests...")
solutions.foreach{ f:(List[(Int, Any)] => List[Any])=>
  assert (f(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ==
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),
    "Given example")
  assert(f(List((1,1),(1,2),(1,3))) == List(1,2,3), "Simple example, no consecutive duplicates")
  assert(f(List((1,1),(1,2),(2,3))) == List(1,2,3,3), "Simple example, 1 consecutive duplicate")
  assert(f(List()) == List(), "Empty input list")
}
println("All tests passed!")

