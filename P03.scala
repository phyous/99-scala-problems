/**
P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
  */

// 1- My Solutions
// Using pattern matching
def nth[A](n: Int, input: List[A]): A = {
  input match {
    case head :: tail if n > 0 => nth(n - 1, tail)
    case head :: tail if n == 0 => head
    case _ => throw new java.util.NoSuchElementException
  }
}

// 2- "Official" Solution optimization
/**
 * I could have used matching on a tuple instead of if. Not sure if that's better or not since I feel my
 */
def nthO[A](n:Int, input:List[A]):A = {
  (n,input) match {
    case (0, head :: _ ) => head
    case (n, _ :: tail) => nthO(n-1,tail)
    case _ => throw new java.util.NoSuchElementException
  }
}

// 3- Tests
println("Starting tests...")
assert(nth(1, List(1, 2, 3)) == 2, "Simple list test (1,2,3)")
assert(nth(1, List(1, 2)) == 2, "Simple list test (1,2)")
try {nth(4, List())} catch {
  case e: java.util.NoSuchElementException => None
  case r: Exception => assert(false, "Empty list test")
}
try {nth(4, List(1,2,3,4))} catch {
  case e: java.util.NoSuchElementException => None
  case r: Exception => assert(false, "n too large")
}
try {nth(-1, List(1,2,3,4))} catch {
  case e: java.util.NoSuchElementException => None
  case r: Exception => assert(false, "n negative")
}
println("All tests passed!")