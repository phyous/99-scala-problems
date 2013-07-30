/**
Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.
Example:

scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  */

// 1- My Solutions
// There is already a splitAt method that does exactly this
def split[A](n: Int, input: List[A]): (List[A], List[A]) = {
  input.splitAt(n)
}

// Let's code our own version. ListBuffer is the right data structure to use here since we need to append to end of list
// in constant time
def split2[A](n: Int, input: List[A]): (List[A], List[A]) = {
  import scala.collection.mutable.ListBuffer
  var buf = new ListBuffer[A]()
  var tailList = input
  if (n > 0) {
    for (i <- 1 to n) {
      buf += tailList.head
      tailList = tailList.tail
    }
  }
  (buf.toList, tailList)
}

// 2- Their solution. Basically, foldRight:
def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
  case (_, Nil)       => (Nil, Nil)
  case (0, list)      => (Nil, list)
  case (n, list) if (n < 0) => (Nil, list) // Had to add this since their code doesn't handle n < 0
  case (n, h :: tail) => {
    val (pre, post) = splitRecursive(n - 1, tail)
    (h :: pre, post)
  }
}
// Their tail recursive solution requires calling reverse on the list.

// 3- Tests
def solutions: List[((Int, List[Any]) => (List[Any],List[Any]))] = List(split, split2, splitRecursive)

println(split2(100, List(1, 2, 3)))

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => (List[Any],List[Any])) =>
    assert(f(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
      (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
      "Given example")
    assert(f(3, List(1, 1, 1)) == (List(1, 1, 1),List()), "n = list size")
    assert(f(100, List(1, 2, 3)) == (List(1, 2, 3),List()), "n > list size")
    assert(f(0, List(1, 2, 3)) == (List(),List(1, 2, 3)), "n == 0")
    assert(f(2, List()) == (List(),List()), "Empty input list")
    assert(f(-1, List(1)) == (List(),List(1)), "N < 0, non-empty list")
    assert(f(-1, List()) == (List(),List()), "N < 0, empty list")
}
println("All tests passed!")