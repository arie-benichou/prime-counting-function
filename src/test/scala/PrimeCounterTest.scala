class PrimeCounterTest extends munit.FunSuite {

  /*
  correctness
  */

  test(s"number of primes before 0 should rise exception") {
    intercept[Exception] {
      PrimeCounter.apply(-1)
    }
  }

  test(s"number of primes until 0 should be 0") {
    assertEquals(PrimeCounter(0), 0)
  }

  test(s"number of primes until 1 should be 0") {
    assertEquals(PrimeCounter(1), 0)
  }

  test(s"number of primes until 2 should be 1") {
    assertEquals(PrimeCounter(2), 1)
  }

  test(s"number of primes until 3 should be 2") {
    assertEquals(PrimeCounter(3), 2)
  }

  test(s"number of primes until 4 should be 2") {
    assertEquals(PrimeCounter(4), 2)
  }

  test(s"number of primes until 5 should be 3") {
    assertEquals(PrimeCounter(5), 3)
  }

  test(s"number of primes until 6 should be 3") {
    assertEquals(PrimeCounter(6), 3)
  }

  test(s"number of primes until 7 should be 4") {
    assertEquals(PrimeCounter(7), 4)
  }

  test(s"number of primes until 8 should be 4") {
    assertEquals(PrimeCounter(8), 4)
  }

  test(s"number of primes until 9 should be 4") {
    assertEquals(PrimeCounter(9), 4)
  }

  test(s"number of primes until 10 should be 4") {
    assertEquals(PrimeCounter(10), 4)
  }

  test(s"number of primes until 11 should be 5") {
    assertEquals(PrimeCounter(11), 5)
  }

  test(s"number of primes until 12 should be 5") {
    assertEquals(PrimeCounter(12), 5)
  }

  test(s"number of primes until 13 should be 6") {
    assertEquals(PrimeCounter(13), 6)
  }

  test(s"number of primes until 14 should be 6") {
    assertEquals(PrimeCounter(14), 6)
  }

  test(s"number of primes until 15 should be 6") {
    assertEquals(PrimeCounter(15), 6)
  }

  test(s"number of primes until 16 should be 6") {
    assertEquals(PrimeCounter(16), 6)
  }

  test(s"number of primes until 17 should be 7") {
    assertEquals(PrimeCounter(17), 7)
  }

  test(s"number of primes until 18 should be 7") {
    assertEquals(PrimeCounter(18), 7)
  }

  test(s"number of primes until 19 should be 8") {
    assertEquals(PrimeCounter(19), 8)
  }

  test(s"number of primes until 20 should be 8") {
    assertEquals(PrimeCounter(20), 8)
  }

  test(s"number of primes until 21 should be 8") {
    assertEquals(PrimeCounter(21), 8)
  }

  test(s"number of primes until 22 should be 8") {
    assertEquals(PrimeCounter(22), 8)
  }

  test(s"number of primes until 23 should be 9") {
    assertEquals(PrimeCounter(23), 9)
  }

  test(s"number of primes until 24 should be 9") {
    assertEquals(PrimeCounter(24), 9)
  }

  test(s"number of primes until 25 should be 9") {
    assertEquals(PrimeCounter(25), 9)
  }

  test(s"number of primes until 26 should be 9") {
    assertEquals(PrimeCounter(26), 9)
  }

  test(s"number of primes until 27 should be 9") {
    assertEquals(PrimeCounter(27), 9)
  }

  test(s"number of primes until 28 should be 9") {
    assertEquals(PrimeCounter(28), 9)
  }

  test(s"number of primes until 29 should be 10") {
    assertEquals(PrimeCounter(29), 10)
  }

  test(s"number of primes until 30 should be 10") {
    assertEquals(PrimeCounter(30), 10)
  }

  test(s"number of primes until 31 should be 11") {
    assertEquals(PrimeCounter(31), 11)
  }

  test(s"number of primes until 100 should be 25") {
    assertEquals(PrimeCounter(100), 25)
  }

  test(s"number of primes until 1 000 should be 168") {
    assertEquals(PrimeCounter(1000), 168)
  }

  test(s"number of primes until 10 000 should be 1 229") {
    assertEquals(PrimeCounter(10000), 1229)
  }

  test(s"number of primes until 100 000 should be 9 592") {
    assertEquals(PrimeCounter(100000), 9592)
  }

  test(s"number of primes until 1 000 000 should be 78 498") {
    assertEquals(PrimeCounter(1000000), 78498)
  }

  /*
  memoization
  */

  test(s"number of primes couples ranges loaded from cache should be 4792") {
    assertEquals(PrimeCounter.loadCache(), 4792)
  }

  test(s"number of primes until 10 000 000 should be 664 579") {
    assertEquals(PrimeCounter(10000000), 664579)
  }

  test(s"number of primes until 100 000 000 should be 5 761 455") {
    assertEquals(PrimeCounter(100000000), 5761455)
  }

  test(s"number of primes until 1 000 000 000 should be 50 847 534") {
    assertEquals(PrimeCounter(1000000000), 50847534)
  }

  test(s"number of primes until 2 000 000 000 should be 98 222 287") {
    assertEquals(PrimeCounter(2000000000), 98222287)
  }

  test(s"number of primes until 2 147 395 600 should be 105 093 480") {
    assertEquals(PrimeCounter(46340 * 46340), 105093480)
  }

  test(s"number of primes until 2 147 483 647 should be 105 097 565") {
    assertEquals(PrimeCounter(Int.MaxValue), 105097565)
  }

  /*
  Integer Limit
  */
  test(s"integer limit awareness") {
    intercept[Exception] {
      PrimeCounter.apply((46340 + 1) * (46340 + 1))
    }
  }

}