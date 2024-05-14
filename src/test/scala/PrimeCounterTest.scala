class PrimeCounterTest extends munit.FunSuite {

  /*
  correctness tests for primes counting
   */

  test("number of primes before 0 should rise exception") {
    intercept[Exception] {
      PrimeCounter(-1)
    }
  }

  test("number of primes until 0 should be 0") {
    assertEquals(PrimeCounter(0), 0)
  }

  test("number of primes until 1 should be 0") {
    assertEquals(PrimeCounter(1), 0)
  }

  test("number of primes until 2 should be 1") {
    assertEquals(PrimeCounter(2), 1)
  }

  test("number of primes until 3 should be 2") {
    assertEquals(PrimeCounter(3), 2)
  }

  test("number of primes until 4 should be 2") {
    assertEquals(PrimeCounter(4), 2)
  }

  test("number of primes until 5 should be 3") {
    assertEquals(PrimeCounter(5), 3)
  }

  test("number of primes until 6 should be 3") {
    assertEquals(PrimeCounter(6), 3)
  }

  test("number of primes until 7 should be 4") {
    assertEquals(PrimeCounter(7), 4)
  }

  test("number of primes until 8 should be 4") {
    assertEquals(PrimeCounter(8), 4)
  }

  test("number of primes until 9 should be 4") {
    assertEquals(PrimeCounter(9), 4)
  }

  test("number of primes until 10 should be 4") {
    assertEquals(PrimeCounter(10), 4)
  }

  test("number of primes until 11 should be 5") {
    assertEquals(PrimeCounter(11), 5)
  }

  test("number of primes until 12 should be 5") {
    assertEquals(PrimeCounter(12), 5)
  }

  test("number of primes until 13 should be 6") {
    assertEquals(PrimeCounter(13), 6)
  }

  test("number of primes until 14 should be 6") {
    assertEquals(PrimeCounter(14), 6)
  }

  test("number of primes until 15 should be 6") {
    assertEquals(PrimeCounter(15), 6)
  }

  test("number of primes until 16 should be 6") {
    assertEquals(PrimeCounter(16), 6)
  }

  test("number of primes until 17 should be 7") {
    assertEquals(PrimeCounter(17), 7)
  }

  test("number of primes until 18 should be 7") {
    assertEquals(PrimeCounter(18), 7)
  }

  test("number of primes until 19 should be 8") {
    assertEquals(PrimeCounter(19), 8)
  }

  test("number of primes until 20 should be 8") {
    assertEquals(PrimeCounter(20), 8)
  }

  test("number of primes until 21 should be 8") {
    assertEquals(PrimeCounter(21), 8)
  }

  test("number of primes until 22 should be 8") {
    assertEquals(PrimeCounter(22), 8)
  }

  test("number of primes until 23 should be 9") {
    assertEquals(PrimeCounter(23), 9)
  }

  test("number of primes until 24 should be 9") {
    assertEquals(PrimeCounter(24), 9)
  }

  test("number of primes until 25 should be 9") {
    assertEquals(PrimeCounter(25), 9)
  }

  test("number of primes until 26 should be 9") {
    assertEquals(PrimeCounter(26), 9)
  }

  test("number of primes until 27 should be 9") {
    assertEquals(PrimeCounter(27), 9)
  }

  test("number of primes until 28 should be 9") {
    assertEquals(PrimeCounter(28), 9)
  }

  test("number of primes until 29 should be 10") {
    assertEquals(PrimeCounter(29), 10)
  }

  test("number of primes until 30 should be 10") {
    assertEquals(PrimeCounter(30), 10)
  }

  test("number of primes until 31 should be 11") {
    assertEquals(PrimeCounter(31), 11)
  }

  test("number of primes until 49 should be 15") {
    assertEquals(PrimeCounter(49), 15)
  }

  test("number of primes until 120 should be ?") {
    assertEquals(PrimeCounter(120), 30)
  }

  test("number of primes until 121 should be ?") {
    assertEquals(PrimeCounter(121), 30)
  }

  test("number of primes until 1331 should be ?") {
    assertEquals(PrimeCounter(1331), 217)
  }

  test("number of primes until 100 should be 25") {
    assertEquals(PrimeCounter(100), 25)
  }

  test("number of primes until 1 000 should be 168") {
    assertEquals(PrimeCounter(1000), 168)
  }

  test("number of primes until 10 000 should be 1 229") {
    assertEquals(PrimeCounter(10000), 1229)
  }

  test("number of primes until 100 000 should be 9 592") {
    assertEquals(PrimeCounter(100000), 9592)
  }

  test("number of primes until 1 000 000 should be 78 498") {
    assertEquals(PrimeCounter(1000000), 78498)
  }

  /*
  correctness tests for prime from order
   */

  test("prime from order < 0 should rise exception") {
    intercept[Exception] {
      PrimeCounter.primeFromOrder(-1)
    }
  }

  test("prime from order 0 should rise exception") {
    intercept[Exception] {
      PrimeCounter.primeFromOrder(0)
    }
  }

  test("prime from order 1 should be 2") {
    assertEquals(PrimeCounter.primeFromOrder(1), 2)
  }

  test("prime from order 2 should be 3") {
    assertEquals(PrimeCounter.primeFromOrder(2), 3)
  }

  test("prime from order 3 should be 5") {
    assertEquals(PrimeCounter.primeFromOrder(3), 5)
  }

  test("prime from order 4 should be 7") {
    assertEquals(PrimeCounter.primeFromOrder(4), 7)
  }

  test("prime from order 5 should be 11") {
    assertEquals(PrimeCounter.primeFromOrder(5), 11)
  }

  test("prime from order 6 should be 13") {
    assertEquals(PrimeCounter.primeFromOrder(6), 13)
  }

  test("prime from order 7 should be 17") {
    assertEquals(PrimeCounter.primeFromOrder(7), 17)
  }

  test("prime from order 8 should be 19") {
    assertEquals(PrimeCounter.primeFromOrder(8), 19)
  }

  test("prime from order 9 should be 23") {
    assertEquals(PrimeCounter.primeFromOrder(9), 23)
  }

  test("prime from order 10 should be 29") {
    assertEquals(PrimeCounter.primeFromOrder(10), 29)
  }

  test("the 168 th prime number should be 997") {
    assertEquals(PrimeCounter.primeFromOrder(168), 997)
  }

  test("the 1 229 th prime number should be 9 973") {
    assertEquals(PrimeCounter.primeFromOrder(1229), 9973)
  }

  test("the 7 000 th prime number should be 70 657") {
    assertEquals(PrimeCounter.primeFromOrder(7000), 70657)
  }

  for (i <- 10 to 1000) {
    val expected = PrimeUtils.findPrimeFromOrder(i)
    test(s"the $i th prime number should be $expected") {
      assertEquals(PrimeCounter.primeFromOrder(i), expected)
    }
  }

  /*
  memoization
   */

  test("number of primes couples ranges loaded from cache should be 4793") {
    assertEquals(PrimeCounter.loadCache(), 4793)
  }

  test("cache already loaded once should not load one more time") {
    assertEquals(PrimeCounter.loadCache(), 0)
  }

  test("number of primes until 10 000 000 should be 664 579") {
    assertEquals(PrimeCounter(10000000), 664579)
  }

  test("number of primes until 79 536 450 should be 4 643 940") {
    assertEquals(PrimeCounter(79536450), 4643940)
  }

  test("number of primes until 100 000 000 should be 5 761 455") {
    assertEquals(PrimeCounter(100000000), 5761455)
  }

  test("number of primes until 1 000 000 000 should be 50 847 534") {
    assertEquals(PrimeCounter(1000000000), 50847534)
  }

  test("number of primes until 2 000 000 000 should be 98 222 287") {
    assertEquals(PrimeCounter(2000000000), 98222287)
  }

  test("number of primes until 2 147 395 600 should be 105 093 480") {
    assertEquals(PrimeCounter(46340 * 46340), 105093480)
  }

  test("number of primes until 2 147 483 647 should be 105 097 565") {
    assertEquals(PrimeCounter(Int.MaxValue), 105097565)
  }

  /*
  Consistency between f(x) and g(x), g(x) beeing the inverse function of f(x)
   */
  for (i <- 10 to 1000) {
    test(s"consistency for x = $i") {
      assertEquals(PrimeCounter(PrimeCounter.primeFromOrder(i)), i)
    }
  }

  /*
  Integer limit for prime counting
   */
  test("integer limit awareness for prime counting") {
    intercept[Exception] {
      PrimeCounter.apply((46340 + 1) * (46340 + 1))
    }
  }

  /*
  Integer limit for prime order
   */
  test("integer limit awareness for prime order") {
    intercept[Exception] {
      PrimeCounter.primeFromOrder(105080512 + 1)
    }
  }

}
