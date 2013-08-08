// P36 (**) Determine the prime factors of a given positive integer (2).
//     Construct a list containing the prime factors and their multiplicity.
//
//     scala> 315.primeFactorMultiplicity
//     res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
//
//     Alternately, use a Map for the result.
//     scala> 315.primeFactorMultiplicity
//     res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)

/*
// One approach is to reuse the solution from P10.
class S99Int(val start: Int) {
  import P10.encode
  def primeFactorMultiplicity: List[(Int,Int)] =
    encode(start.primeFactors) map { _.swap }
}
*/

// But we can do it directly.
class S99Int(val start: Int) {
  def primeFactorMultiplicity: Map[Int,Int] = {
    def factorCount(n: Int, p: Int): (Int,Int) = 
      if (n % p != 0) (0, n)
      else factorCount(n / p, p) match { case (c, d) => (c + 1, d) }
    def factorsR(n: Int, ps: Stream[Int]): Map[Int, Int] = 
      if (n == 1) Map()
      else if (n.isPrime) Map(n -> 1)
      else {
        val nps = ps.dropWhile(n % _ != 0)
        val (count, dividend) = factorCount(n, nps.head)
        Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
      }
    factorsR(start, primes)
  }
  
  // This also lets us change primeFactors.
  def primeFactors: List[Int] =
    start.primeFactorMultiplicity flatMap { v => List.make(v._2, v._1) } toList
}
