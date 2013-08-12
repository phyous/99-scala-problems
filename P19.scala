/**
Rotate a list N places to the left.
Examples:
scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  */

// 1- My solution
def rotate(n: Int, input: List[Any]): List[Any] = {
  val length = input.length
  val absN = if (n < 0) n + length else n
  if (absN > length || absN < 0) return input // Check if n is out of bounds
  val (a, b) = input.splitAt(absN)
  b ::: a
}

def solutions: List[((Int, List[Any]) => List[Any])] = List(rotate)

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => List[Any]) =>
    assert(f(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c), "Given example1")
    assert(f(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i), "Given example2")
    assert(f(-2, List()) == List(), "empty list")
    assert(f(100, List(1, 2)) == List(1, 2), "n > list.length returns original")
}
println("All tests passed!")