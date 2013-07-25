/**
Duplicate the elements of a list a given number of times.
Example:
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  */

// 1- My solution
def duplicate[A](n: Int, input: List[A]): List[A] = {
  if (n < 0) return Nil
  input.flatMap { e => List.fill(n)(e) }
}

// 2- Their Solution
// Same solution except use fill instead of deprecated make

// 3- Tests
def solutions: List[((Int, List[Any]) => List[Any])] = List(duplicate)

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => List[Any]) =>
    assert(f(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd), "Given example")
    assert(f(1, List(1, 1, 1)) == List(1, 1, 1), "Same element duplicated once")
    assert(f(2,List()) == List(), "Empty input list")
    assert(f(-1,List()) == List(), "N < 0")
}
println("All tests passed!")