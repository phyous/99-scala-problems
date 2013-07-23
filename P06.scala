/**
P06 (*) Find out whether a list is a palindrome.
Example:
scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true
  */

// 1- My Solutions
// 2n time (n for fold, n for equals), n space (creating extra List)
def isPalindrome[A](input:List[A]):Boolean = {
  input.foldLeft(List[A]())((l,e) => e :: l).equals(input)
}

// 2- "Official" Solution optimization
// My solution matches

// 3- Tests
def solutions[A]:List[(List[A] => Boolean)] = List(isPalindrome)

println("Starting tests...")
solutions.foreach{ f:(List[Any] => Boolean)=>
  assert (f(List(1,2,1)) == true, "Simple list test (1,2,1)")
  assert (f(List(1,2,3)) == false, "Simple list test (1,2,3)")
  assert (f(List('a','a','a')) == true, "All same elements")
  assert (f(List()) == true, "Empty List()")
}
println("All tests passed!")