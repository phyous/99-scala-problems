import scala.util.Random

/**
Generate a random permutation of the elements of a list.
Hint: Use the solution of problem P23.
Example:

scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  */

// 1- My solutions

// Can use randomSelect in P20 until input list is empty. Will code up a similar recursive solution here.
def removeAt[A](n: Int, in: List[A]): (List[A], A) = {
  val (a, b) = in.splitAt(n)
  (a ::: b.tail, b.head)
}

def randomPermute(input: List[Any]): List[Any] = {
  def permuteRecursive(in: List[Any], length: Int): List[Any] = {
    in match {
      case Nil => Nil
      case e => {
        val ret = removeAt(Random.nextInt(length), e)
        ret._2 :: permuteRecursive(ret._1, length-1)
      }
    }
  }

  permuteRecursive(input, input.length)
}

// 3- Tests
println("Starting tests...")
val in = List('a, 'b, 'c, 'd, 'e, 'f)
val ret = randomPermute(in)

// Same length list is returned
assert(ret.length == in.length)
// All elements returned are in the original set
for(i <- ret) assert(in.contains(i))

println("All tests passed!")
