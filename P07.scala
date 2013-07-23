/**
Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  */

def flatten(input: List[Any]): List[Any] = {
  input.flatMap {
    case list: List[_] => flatten(list)
    case e => List(e)
  }
}

def solutions:List[(List[Any] => List[Any] )] = List(flatten)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => List[Any])=>
  assert (f(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8), "Simple list test (1,1,2,3,5,8)")
}
println("All tests passed!")