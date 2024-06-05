import scala.annotation.tailrec

// TODO write unit tests

object PrimesUtils {

  def isPrime(n: Long): Boolean = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n == 1) return false
    if (n == 2) return true
    if (n == 3) return true
    if (n % 2 == 0) return false
    @tailrec
    def _isPrime(i: Long): Boolean = {
      if (i * i > n) return true
      if (n % i == 0) return false
      _isPrime(i + 2)
    }
    _isPrime(3)
  }

  def findPrimeAfter(n: Long): Long = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n <= 1) return 2
    if (n == 2) return 3
    @tailrec
    def _findPrimeAfter(x: Long): Long = {
      if (isPrime(x)) return x
      _findPrimeAfter(x + 2)
    }
    val oddAlignment = if (n % 2 == 0) 1 else 2
    _findPrimeAfter(n + oddAlignment)
  }

  def findPrimeBefore(n: Long): Long = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n < 3) return n // by convention.. could return 0 or -1 also
    if (n == 3) return 2
    @tailrec
    def _findPrimeBefore(x: Long): Long = {
      if (isPrime(x)) return x
      _findPrimeBefore(x - 2)
    }
    val oddAlignment = if (n % 2 == 0) 1 else 2
    _findPrimeBefore(n - oddAlignment)
  }

  def countPrimesUntil(n: Long): Long = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n < 2) return 0
    if (n == 2) return 1
    var counter = 1L
    for (x <- 3L to n by 2 if isPrime(x)) counter += 1
    counter
  }

  def findPrimeFromRank(n: Long): Long = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n == 0) throw new Exception("order 0 is not defined.")
    if (n == 1) return 2
    if (n == 2) return 3
    var counter = 2L
    var x = 3L
    while (counter < n) {
      x += 2
      if (isPrime(x)) counter += 1
    }
    x
  }

  def getPrimesUntil(n: Long): Seq[Long] = {
    if (n < 0) throw new Exception(s"$n is not a valid integer !")
    if (n < 2) return Nil
    if (n < 3) return List(2)
    for (x <- 3L to n by 2 if isPrime(x)) yield x
  }

  def getPrimesBetween(start: Long, end: Long): Seq[Long] = {
    if (start < 0) throw new Exception(s"$start is not a valid integer !")
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    for (x <- start to end if isPrime(x)) yield x
  }

  def isMultipleOf2357(n: Long): Boolean = {
    for (p <- List(2, 3, 5, 7)) if (n % p == 0) return true
    false
  }

  def countMultiplesOf2357(start : Long, end: Long): Long = {
    var counter = 0
    for (n <- start to end if isMultipleOf2357(n)) counter += 1
    counter
  }

}
