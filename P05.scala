/**
P05 (*) Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  */

// 1- My Solutions
// The standard way of solving this in scala
def reverse1[A](input: List[A]): List[A] = {
  input.foldLeft(List[A]())((l, r) => r :: l)
}

// Use a stack
def reverse2[A](input: List[A]): List[A] = {
  import scala.collection.immutable.Stack
  input.foldLeft(Stack[A]())((c, r) => c.push(r)).toList
}

// 3- Tests
val solutions: List[(List[Int] => List[Int])] = List(reverse1, reverse2)

println("Starting tests...")
solutions.foreach {
  f =>
    assert(f(List(1, 2, 3)).equals(List(3, 2, 1)), "Simple list test (1,2,3)")
    assert(f(List()).equals(List()), "Empty list")
}
println("All tests passed!")