import scala.annotation.tailrec

// TODO write unit tests

object PrimeUtils {

  def isPrime(n: Int): Boolean = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n == 1) return false
    if (n == 2) return true
    if (n == 3) return true
    if (n % 2 == 0) return false
    @tailrec
    def _isPrime(i: Int): Boolean = {
      if (i * i > n) return true
      if (n % i == 0) return false
      _isPrime(i + 2)
    }
    _isPrime(3)
  }

  def findPrimeAfter(n: Int): Int = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n <= 1) return 2
    if (n == 2) return 3
    @tailrec
    def _findPrimeAfter(x: Int): Int = {
      if (isPrime(x)) return x
      _findPrimeAfter(x + 2)
    }
    val oddAlignment = if (n % 2 == 0) 1 else 2
    _findPrimeAfter(n + oddAlignment)
  }

  def findPrimeBefore(n: Int): Int = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n < 3) return n // by convention.. could return 0 or -1 also
    if (n == 3) return 2
    @tailrec
    def _findPrimeBefore(x: Int): Int = {
      if (isPrime(x)) return x
      _findPrimeBefore(x - 2)
    }
    val oddAlignment = if (n % 2 == 0) 1 else 2
    _findPrimeBefore(n - oddAlignment)
  }

  def countPrimesUntil(n: Int): Int = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n < 2) return 0
    if (n == 2) return 1
    var counter = 1
    for (x <- 3 to n by 2 if isPrime(x)) counter += 1
    counter
  }

  def getPrimesUntil(n: Int): Seq[Int] = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n < 2) return Nil
    if (n < 3) return List(2)
    for (x <- 3 to n by 2 if isPrime(x)) yield x
  }

  def getPrimesBetween(start: Int, end: Int): Seq[Int] = {
    if (start < 0) throw new Exception(s"$start is not a valid integer !")
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    for (x <- start to end if isPrime(x)) yield x
  }

}
