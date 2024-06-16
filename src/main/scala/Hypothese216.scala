import PrimesUtils.{isPrime, findPrimeBefore, findPrimeAfter}

// (1,2,3), (7,11,13), (31,37,41), (1321,1327,1361)
// p1 x p2 + 1 = p3

// (2,3,5), (3,5,7), (17,19,23)
// p1 x p2 - 1 = p3
object Hypothese216 {

  // until 100000000 : none other found
  def main(args: Array[String]): Unit = {
    val set = scala.collection.mutable.SortedSet[(Long, Long, Long)]()
    var p1 = 1L
    var p2 = findPrimeAfter(p1)
    while (p1 < 10000000L) {
      println(s"$p1")
      val p3 = findPrimeAfter(p2)
      val test = (p1 * p2 - 1) % p3 == 0
      if (test) {
        println(p1, p2, p3)
        set.add(p1, p2, p3)
      }
      p1 = p2
      p2 = p3
    }
    println(set)
  }

  // (1,2,3), (7,11,13), (31,37,41), (1321,1327,1361)
  def __main(args: Array[String]): Unit = {

    def align(n: Long) = {
      if (n == 0 || n % 13 == 0) n // ?? n % 13 specific to a
      else if (n % 2 == 0) n - 1
      else if (n % 5 == 0) n - 2
      else n
    }

    def check_a(n: Long): Boolean = {
      val aligned = align(n)
      if (aligned == 1) return true
      if (PrimesUtils.isPrime(aligned)) return true
      if (aligned % 13 == 0) return true
      if (aligned % 7 == 0) return true
      if (aligned % 37 == 0) return true
      if (aligned % 5 == 0) return true
      if (aligned % 29 == 0) return true
      if (aligned % 43 == 0) return true
      if (aligned % 17 == 0) return true
      if (aligned % 11 == 0) return true // not sure
      if (aligned % 31 == 0) return true
      if (aligned % 19 == 0) return true
      if (aligned % 3 == 0) return true // not sure
      if (aligned % 23 == 0) return true // not sure
      false
    }

    var n = 1361L
    while (n < 2311L) {
      if (PrimesUtils.isPrime(n)) {
        // if (!PrimesUtils.isPrime(n) && Primes2357.doesNotdivide(n)) {
        println(n)
        val a = n % 1361
        println(a, align(a), check_a(a))

        // if (!check_a(a)) return

        val b = n % 41
        println(b, align(b))
        val c = n % 13
        println(c, align(c))
        val d = n % 3
        println(d, align(d))
        if (PrimesUtils.isPrime(n))
          println("prime")
        println
      }

      n += 1
    }
  }

}
