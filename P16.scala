import scala.annotation.tailrec

/**
Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */

// 1- My Solutions
// Tail recursive solution. Sucks that we have to reverse at the end.
def drop[A](n: Int, input: List[A]): List[A] = {
  @tailrec
  def dropIdx(i:Int, ittList:List[A], newList:List[A]):List[A] = {
    if(ittList.isEmpty) newList
    else if(i % n == 0) dropIdx(i+1, ittList.tail, newList)
    else dropIdx(i+1, ittList.tail, ittList.head :: newList)
  }
  dropIdx(1, input, List.empty).reverse
}

// grouped is a neat method that partitions elements in fixed size groups
// We're iterating over the list too many times for my liking though
def drop2[A](n: Int, input: List[A]): List[A] = {
  val (splitLeft, splitRight) = input.splitAt(n-1)
  if(splitLeft.isEmpty) return Nil // If the left split is empty, n was probably < 1
  val groupedLeft = splitRight.grouped(n).toList.flatMap( e => e.tail )
  splitLeft ::: groupedLeft
}

// 2- Their Solution
// Same as my tailrec solution, except they move where reverse happens

// 3- Tests
def solutions: List[((Int, List[Any]) => List[Any])] = List(drop, drop2)

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => List[Any]) =>
    assert(f(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k), "Given example")
    assert(f(2, List(1, 1, 1)) == List(1, 1), "list with duplicates")
    assert(f(5, List(1, 2, 3)) == List(1, 2, 3), "n > list size")
    assert(f(2, List()) == List(), "Empty input list")
    assert(f(-1, List(1)) == List(), "N < 0, empty list")
    assert(f(-1, List()) == List(), "N < 0, non-empty list")
}
println("All tests passed!")