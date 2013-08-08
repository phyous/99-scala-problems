// P33 (*) Determine whether two positive integer numbers are coprime.
//     Two numbers are coprime if their greatest common divisor equals 1.
//
//     scala> 35.isCoprimeTo(64)
//     res0: Boolean = true

class S99Int(val start: Int) {
  def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1
}
