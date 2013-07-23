/**
P04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
  */

// 1- My Solutions
// 'Regular' recursive solution
def length1[A](input: List[A]): Int = {
  input match {
    case _ :: tail => 1 + length1(tail)
    case _ => 0
  }
}

// Let's try with tail recursion
def length2[A](input: List[A]): Int = {
  def recurse[A](sum: Int, l: List[A]): Int = {
    l match {
      case _ :: tail => recurse(sum + 1, tail)
      case _ => sum
    }
  }
  recurse(0, input)
}

// 2- "Official" Solution optimization
// My solutions check out with official ones. Could have also used foldLeft

// 3- Tests
val solutions: List[(List[Int] => Int)] = List(length1 _, length2 _)

println("Starting tests...")
solutions.foreach {
  f =>
    assert(f(List(1, 2, 3)) == 3, "Simple list test (1,2,3)")
    assert(f(List()) == 0, "Empty list")
}
println("All tests passed!")