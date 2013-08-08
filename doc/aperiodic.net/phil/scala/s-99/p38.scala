// P38 (*) Compare the two methods of calculating Euler's totient function.
//     Use the solutions of problems P34 and P37 to compare the algorithms.  Try
//     to calculate phi(10090) as an example.

// Here's an object that will test the relative execution times of the two
// approaches.
// On a 2.4 GHz Athlon 64 X2, here's what happens the first time `test` is called:
//   Preload primes: 20 ms.
//   P34 (10090): 65 ms.
//   P37 (10090): 3 ms.
//
// The JVM tends to profile its execution, though.  Here's a several-iteration run.
//   scala> import P38._
//   import P38._
//   
//   scala> test(10090)
//   Preload primes: 9 ms.
//   P34 (10090): 53 ms.
//   P37 (10090): 4 ms.
//   
//   scala> test(10090)
//   Preload primes: 2 ms.
//   P34 (10090): 28 ms.
//   P37 (10090): 1 ms.
//   
//   scala> test(10090)
//   Preload primes: 2 ms.
//   P34 (10090): 17 ms.
//   P37 (10090): 1 ms.
//   
//   scala> test(10090)
//   Preload primes: 2 ms.
//   P34 (10090): 3 ms.
//   P37 (10090): 0 ms.
//   
//   scala> test(10090)
//   Preload primes: 4 ms.
//   P34 (10090): 2 ms.
//   P37 (10090): 0 ms.

object P38 {
  import arithmetic.S99Int._

  def time[A](label: String)(block: => A): A = {
    val now = System.currentTimeMillis()
    val ret = block
    println(label + ": " + (System.currentTimeMillis() - now) + " ms.")
    ret
  }
  
  def test(n: Int) {
    time("Preload primes") {
      primes takeWhile { _ <= Math.sqrt(n) } force
    }
    time("P34 (" + n + ")") {
      n.totientP34
    }
    time("P37 (" + n + ")") {
      n.totient
    }
  }
}
