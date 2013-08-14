import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Builder}

/**
Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
Example:

scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  */

// 1- My solutions

// Tail recursive solution which is not terribly efficient. We're calling .length a bunch(O(N)) in a loop
def combinations[A](n: Int, in: List[A]): List[List[A]] = {
  import mutable.ListBuffer
  def collector(current: List[A], collected: List[List[A]]): List[List[A]] = {
    current match {
      case Nil => collected.foldLeft(List[List[A]]())((c, r) => if (r.length == n) r :: c else c)
      case head :: tail => {
        var newCollected = collected.to[ListBuffer]
        for (i <- collected if i.length < n) newCollected += (i ::: List(head))
        newCollected += List(head)
        collector(tail, newCollected.toList)
      }
    }
  }

  collector(in, List())
}

// 2- Their solution
def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
  ls match {
    case Nil => Nil
    case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
  }

def combinations2[A](n: Int, ls: List[A]): List[List[A]] =
  if (n == 0) List(Nil)
  else flatMapSublists(ls) { sl =>
    combinations2(n - 1, sl.tail) map {sl.head :: _}
  }

// 3- Tests
def solutions: List[((Int, List[Any]) => (List[List[Any]]))] = List(combinations, combinations2)

println("Starting tests...")
solutions.foreach {
  f: ((Int, List[Any]) => (List[List[Any]])) =>
    val ret = f(3, List('a, 'b, 'c, 'd))
    val expected = List(List('b, 'c, 'd), List('a, 'c, 'd), List('a, 'b, 'd), List('a, 'b, 'c))
    ret.foreach(r => assert(expected.contains(r)))
}
println("All tests passed!")
