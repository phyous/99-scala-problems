// P39 (*) A list of prime numbers.
//     Given a range of integers by its lower and upper limit, construct a list
//     of all prime numbers in that range.
//
//     scala> listPrimesinRange(7 to 31)
//     res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)

object S99Int {
  def listPrimesinRange(r: Range): List[Int] =
    primes dropWhile { _ < r.first } takeWhile { _ <= r.last } toList
}
