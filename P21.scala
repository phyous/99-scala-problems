/**
Insert an element at a given position into a list.
Example:
scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  */

// 1- My solutions
def insertAt[A](e:A, pos:Int, input:List[A]):List[A] ={
  val (a,b) = input.splitAt(pos)
  a ::: List(e) ::: b
}

println(insertAt('new, 4, List('a, 'b, 'c, 'd)))

// 3- Tests
def solutions: List[(Any, Int, List[Any]) => List[Any]] = List(insertAt)

println("Starting tests...")
solutions.foreach {
  f: ((Any, Int, List[Any]) => List[Any]) =>
    assert(f('new, 1, List('a, 'b, 'c, 'd)) ==  List('a, 'new, 'b, 'c, 'd), "given example")
    assert(f('new, 4, List('a, 'b, 'c, 'd)) ==  List('a, 'b, 'c, 'd, 'new), "Add to end of list")
    assert(f('new, 100, List('a, 'b, 'c, 'd)) ==  List('a, 'b, 'c, 'd, 'new), "n > list length")
}
println("All tests passed!")