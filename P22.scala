/**
Create a list containing all integers within a given range.
Example:
scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  */

// 1- My solutions
// Tail recursion, if-else
def range(i: Int, j: Int): List[Int] = {
  def collectRange(e: Int, c: List[Int]): List[Int] = {
    if (e >= i && e <= j) collectRange(e + 1, e :: c)
    else c
  }
  if (i > j) return List()
  else collectRange(i, List()).reverse
}

// use range class in scala
def range2(i: Int, j: Int): List[Int] = (i until j+1).toList

// normal recursion, pattern matching
def range3(i: Int, j: Int): List[Int] = {
  (i, j) match {
    case (a, b) if (a > b) => Nil
    case (a, _) => a :: range3(i + 1, j)
  }
}

// 3- Tests
def solutions: List[(Int, Int) => List[Int]] = List(range, range2, range3)

println("Starting tests...")
solutions.foreach {
  f: ((Int, Int) => List[Int]) =>
    assert(f(4, 9) == List(4, 5, 6, 7, 8, 9), "given example")
}
println("All tests passed!")