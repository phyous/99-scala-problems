/**
Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:

scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
  */

// 1- My solutions
// Tail recursive
def slice(i: Int, j: Int, input: List[Any]): List[Any] = {
  def collect(idx: Int, tmpList: List[Any], collected: List[Any]): List[Any] = {
    tmpList match {
      case Nil => List()
      case head :: tail => {
        if (idx >= i && idx < j)
          collect(idx + 1, tail, head :: collected)
        else if (idx >= j)
          collected
        else
          collect(idx + 1, tail, collected)
      }
    }
  }

  collect(0, input, List()).reverse
}

// "normal recursive"
def slice2(i: Int, j: Int, input: List[Any]): List[Any] = {
  def collect(idx: Int, tmpList: List[Any]): List[Any] = {
    tmpList match {
      case Nil => List()
      case head :: tail => {
        if (idx >= i && idx < j)
          head :: collect(idx + 1, tail)
        else if (idx >= j)
          Nil
        else
          collect(idx + 1, tail)
      }
    }
  }
  collect(0, input)
}

// Try with foldLeft -- because, why not?
def slice3(i: Int, j: Int, input: List[Any]): List[Any] = {
  input.foldLeft(0, List[Any]())((c, in) =>
    if (c._1 >= i && c._1 < j) (c._1 + 1, in :: c._2)
    else if (c._1 >= j) c
    else (c._1 + 1, c._2)
  )._2.reverse
}

// 3- Tests
def solutions: List[(Int, Int, List[Any]) => List[Any]] = List(slice, slice2, slice3)

println("Starting tests...")
solutions.foreach {
  f: ((Int, Int, List[Any]) => List[Any]) =>
    assert(f(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g), "Given example")
    assert(f(10,100, List())== List(), "Empty list")
}
println("All tests passed!")