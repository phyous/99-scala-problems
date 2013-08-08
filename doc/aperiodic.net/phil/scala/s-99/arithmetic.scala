package arithmetic {

  class S99Int(val start: Int) {
    import S99Int._

    // P31
    def isPrime: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })

    // P33
    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1

    // P34
    def totientP34: Int = (1 to start) filter { start.isCoprimeTo(_) } length

    // P37
    def totient: Int = start.primeFactorMultiplicity.foldLeft(1) { (r, f) =>
      f match { case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt }
    }

    // P35 (amended by P36)
    def primeFactors: List[Int] =
      start.primeFactorMultiplicity flatMap { v => List.make(v._2, v._1) } toList

    // P36
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

    // P40
    def goldbach: (Int,Int) =
      primes takeWhile { _ < start } find { p => (start - p).isPrime } match {
        case None     => throw new IllegalArgumentException
        case Some(p1) => (p1, start - p1)
      }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // P31
    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

    // P32
    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

    // P39
    def listPrimesinRange(r: Range): List[Int] =
      primes dropWhile { _ < r.first } takeWhile { _ <= r.last } toList

    // P41
    def printGoldbachList(r: Range) {
      printGoldbachListLimited(r, 0)
    }

    // P41
    def printGoldbachListLimited(r: Range, limit: Int) {
      (r filter { n => n > 2 && n % 2 == 0 } map { n => (n, n.goldbach) }
       filter { _._2._1 >= limit } foreach {
         _ match { case (n, (p1, p2)) => println(n + " = " + p1 + " + " + p2) }
       })
    }
  }
}
