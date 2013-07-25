/**
Duplicate the elements of a list.
Example:
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  */

// 1- My solution
def duplicate[A](input:List[A]):List[A] = input.flatMap{ e => List.fill(2)(e)}

// 2- Their Solution
// Same solution

// 3- Tests
def solutions:List[(List[Any] => List[Any] )] = List(duplicate)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => List[Any])=>
  assert (f(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd), "Given example")
  assert (f(List(1,1,1)) == List(1,1,1,1,1,1), "Same element")
  assert(f(List()) == List(), "Empty input list")
}
println("All tests passed!")