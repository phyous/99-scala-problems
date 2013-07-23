/**
P02 (*) Find the last but one element of a list.
--[phyous] I assume this means second to last
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
  */

// 1- My Solution
def penultimate[A](input:List[A]):A = {
  input match {
    case List() => throw new java.util.NoSuchElementException()
    case head :: Nil => throw new java.util.NoSuchElementException()
    case head :: tail if tail.tail == Nil => head
    case head :: tail => penultimate(tail)
  }
}

// 2- "Official" Solution optimization
/**
 * I could have made pattern match slightly more efficient (3 clauses instead of 4)
 */
def penultimateO[A](input:List[A]):A  = {
  input match {
    case l1 :: _ :: Nil => l1
    case _ :: tail => penultimateO(tail)
    case _  => throw java.util.NoSuchElementException
  }
}

// 3- Tests
println("Starting tests...")
assert(penultimate(List(1,2,3,4)) == 3, "Basic case")
assert(penultimate(List('a','b')) == 'a', "Try a list of characters")
try { penultimate(List()) } catch {
  case e: java.util.NoSuchElementException => None
  case r: Exception => assert(false, "Empty list test")
}
try { penultimate(List(1)) } catch {
  case e: java.util.NoSuchElementException => None
  case r: Exception => assert(false, "Single element test case")
}
println("All tests passed!")