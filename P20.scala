/**
Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:

scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  */

// 1- My solution
def removeAt[A](n: Int, input: List[A]): (List[A], A) = {
  val (a, b) = input.splitAt(n)
  (a ::: b.tail, b.head)
}

// 3- Tests
def solutions: List[((Int, List[Any]) => (List[Any], Any))] = List(removeAt)

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => (List[Any], Any)) =>
    assert(f(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b), "given example")
}
println("All tests passed!")