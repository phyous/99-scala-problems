import scala.util.Random

/**
Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
Hint: Use the solution to problem P20
  */

// 1- My solutions
def removeAt[A](n: Int, input: List[A]): (List[A], A) = {
  val (a, b) = input.splitAt(n)
  (a ::: b.tail, b.head)
}

// Iterative solution
def randomSelect[A](n: Int, input: List[A]): List[A] = {
  var (oldList, newList) = (input, List[A]())

  for (i <- 0 until n) {
    val randomIndex = Random.nextInt(oldList.length)
    val ret = removeAt(randomIndex, oldList)
    oldList = ret._1
    newList = ret._2 :: newList
  }
  newList
}

// Tail recursive solution
def randomSelect2[A](n: Int, input: List[A]): List[A] = {
  val length = input.length
  def collect(count: Int, oldList: List[A], newList: List[A]): List[A] = {
    val randomIndex = Random.nextInt(length - n)
    count match {
      case i if i >= n => newList
      case i => {
        val ret = removeAt(randomIndex, oldList)
        collect(i + 1, ret._1, ret._2 :: newList)
      }
    }
  }

  collect(0, input, List())
}

// 3- Tests
def solutions: List[(Int, List[Any]) => List[Any]] = List(randomSelect, randomSelect2)

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => List[Any]) =>
    val input = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val n = 3
    val result = f(n, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    assert(result.length == n, "given example, length check")
    for(i <- result) {
      assert(input.contains(i), "given example, set check %s".format(i))
    }
}
println("All tests passed!")