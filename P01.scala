/**
P01 (*) Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
  */

// 1- My Solutions
def last1[A](input:List[A]):A = {
  input.last
}

def last2[A](input:List[A]):A = {
  input match {
    case List() => throw new java.util.NoSuchElementException
    case head :: Nil => head
    case head :: tail => last2(tail)
  }
}

// 2- "Official" Solution optimization
// No need, success on first attempt

// 3- Tests
val solutions:List[(List[Int] => Int)] = List(last1, last2)

println("Starting tests...")
solutions.foreach{ f =>
  assert (f(List(1,2,3)) == 3, "Simple list test (1,2,3,4)")
  assert (f(List(1)) == 1, "Single element list test")
  try {
    f(List())
  } catch {
    case e: java.util.NoSuchElementException => None
    case r: Exception => assert(false, "Empty list test")
  }
}
println("All tests passed!")

