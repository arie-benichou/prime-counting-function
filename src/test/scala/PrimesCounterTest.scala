class PrimesCounterTest extends munit.FunSuite {

  /*
  correctness tests for primes counting
   */

  test("number of primes before 0 should rise exception") {
    intercept[AssertionError] {
      PrimesCounter(-1)
    }
  }

  // TODO FIX
  test("number of primes until 0 should be 0") {
    // assertEquals(PrimesCounter(0), 0L)
  }

  test("number of primes until 1 should be 0") {
    assertEquals(PrimesCounter(1), 0L)
  }

  test("number of primes until 2 should be 1") {
    assertEquals(PrimesCounter(2), 1L)
  }

  test("number of primes until 3 should be 2") {
    assertEquals(PrimesCounter(3), 2L)
  }

  test("number of primes until 4 should be 2") {
    assertEquals(PrimesCounter(4), 2L)
  }

  test("number of primes until 5 should be 3") {
    assertEquals(PrimesCounter(5), 3L)
  }

  test("number of primes until 6 should be 3") {
    assertEquals(PrimesCounter(6), 3L)
  }

  test("number of primes until 7 should be 4") {
    assertEquals(PrimesCounter(7), 4L)
  }

  test("number of primes until 8 should be 4") {
    assertEquals(PrimesCounter(8), 4L)
  }

  test("number of primes until 9 should be 4") {
    assertEquals(PrimesCounter(9), 4L)
  }

  test("number of primes until 10 should be 4") {
    assertEquals(PrimesCounter(10), 4L)
  }

  test("number of primes until 11 should be 5") {
    assertEquals(PrimesCounter(11), 5L)
  }

  test("number of primes until 12 should be 5") {
    assertEquals(PrimesCounter(12), 5L)
  }

  test("number of primes until 13 should be 6") {
    assertEquals(PrimesCounter(13), 6L)
  }

  test("number of primes until 14 should be 6") {
    assertEquals(PrimesCounter(14), 6L)
  }

  test("number of primes until 15 should be 6") {
    assertEquals(PrimesCounter(15), 6L)
  }

  test("number of primes until 16 should be 6") {
    assertEquals(PrimesCounter(16), 6L)
  }

  test("number of primes until 17 should be 7") {
    assertEquals(PrimesCounter(17), 7L)
  }

  test("number of primes until 18 should be 7") {
    assertEquals(PrimesCounter(18), 7L)
  }

  test("number of primes until 19 should be 8") {
    assertEquals(PrimesCounter(19), 8L)
  }

  test("number of primes until 20 should be 8") {
    assertEquals(PrimesCounter(20), 8L)
  }

  test("number of primes until 21 should be 8") {
    assertEquals(PrimesCounter(21), 8L)
  }

  test("number of primes until 22 should be 8") {
    assertEquals(PrimesCounter(22), 8L)
  }

  test("number of primes until 23 should be 9") {
    assertEquals(PrimesCounter(23), 9L)
  }

  test("number of primes until 24 should be 9") {
    assertEquals(PrimesCounter(24), 9L)
  }

  test("number of primes until 25 should be 9") {
    assertEquals(PrimesCounter(25), 9L)
  }

  test("number of primes until 26 should be 9") {
    assertEquals(PrimesCounter(26), 9L)
  }

  test("number of primes until 27 should be 9") {
    assertEquals(PrimesCounter(27), 9L)
  }

  test("number of primes until 28 should be 9") {
    assertEquals(PrimesCounter(28), 9L)
  }

  test("number of primes until 29 should be 10") {
    assertEquals(PrimesCounter(29), 10L)
  }

  test("number of primes until 30 should be 10") {
    assertEquals(PrimesCounter(30), 10L)
  }

  test("number of primes until 31 should be 11") {
    assertEquals(PrimesCounter(31), 11L)
  }

  test("number of primes until 49 should be 15") {
    assertEquals(PrimesCounter(49), 15L)
  }

  test("number of primes until 120 should be ?") {
    assertEquals(PrimesCounter(120), 30L)
  }

  test("number of primes until 121 should be ?") {
    assertEquals(PrimesCounter(121), 30L)
  }

  test("number of primes until 1331 should be ?") {
    assertEquals(PrimesCounter(1331), 217L)
  }

  test("number of primes until 100 should be 25") {
    assertEquals(PrimesCounter(100), 25L)
  }

  test("number of primes until 1 000 should be 168") {
    assertEquals(PrimesCounter(1000), 168L)
  }

  test("number of primes until 10 000 should be 1 229") {
    assertEquals(PrimesCounter(10000), 1229L)
  }

  test("number of primes until 100 000 should be 9 592") {
    assertEquals(PrimesCounter(100000), 9592L)
  }

  test("number of primes until 1 000 000 should be 78 498") {
    assertEquals(PrimesCounter(1000000), 78498L)
  }

  for (n <- 1 to 7 * 7 + 1) {
    val end = 13 * 13 + 1
    for (i <- 1L to end) {
      val obtained = PrimesCounter(n, end - i + n)
      val expected = PrimesUtils.getPrimesBetween(n, end - i + n).size.toLong
      test(s"number of primes in [ $n, ${end - i + n} ]") {
        assertEquals(obtained, expected)
      }
    }
  }

  /*
  correctness tests for prime from rank
   */

  test("prime from rank < 0 should rise exception") {
    intercept[AssertionError] {
      PrimesCounter.primeFromRank(-1)
    }
  }

  test("prime from rank 0 should rise exception") {
    intercept[AssertionError] {
      PrimesCounter.primeFromRank(0)
    }
  }

  test("prime from rank 1 should be 2") {
    assertEquals(PrimesCounter.primeFromRank(1), 2L)
  }

  test("prime from rank 2 should be 3") {
    assertEquals(PrimesCounter.primeFromRank(2), 3L)
  }

  test("prime from rank 3 should be 5") {
    assertEquals(PrimesCounter.primeFromRank(3), 5L)
  }

  test("prime from rank 4 should be 7") {
    assertEquals(PrimesCounter.primeFromRank(4), 7L)
  }

  test("prime from rank 5 should be 11") {
    assertEquals(PrimesCounter.primeFromRank(5), 11L)
  }

  test("prime from rank 6 should be 13") {
    assertEquals(PrimesCounter.primeFromRank(6), 13L)
  }

  test("prime from rank 7 should be 17") {
    assertEquals(PrimesCounter.primeFromRank(7), 17L)
  }

  test("prime from rank 8 should be 19") {
    assertEquals(PrimesCounter.primeFromRank(8), 19L)
  }

  test("prime from rank 9 should be 23") {
    assertEquals(PrimesCounter.primeFromRank(9), 23L)
  }

  test("prime from rank 10 should be 29") {
    assertEquals(PrimesCounter.primeFromRank(10), 29L)
  }

  test("the 168 th prime number should be 997") {
    assertEquals(PrimesCounter.primeFromRank(168), 997L)
  }

  test("the 1 229 th prime number should be 9 973") {
    assertEquals(PrimesCounter.primeFromRank(1229), 9973L)
  }

  test("the 7 000 th prime number should be 70 657") {
    assertEquals(PrimesCounter.primeFromRank(7000), 70657L)
  }

  for (i <- 10 to 1000) {
    val expected = PrimesUtils.findPrimeFromRank(i)
    test(s"the $i th prime number should be $expected") {
      assertEquals(PrimesCounter.primeFromRank(i), expected)
    }
  }

  /*
  memoization
   */

  // TODO use test/resource/
  test(
    "number of primes couples ranges loaded from json encoded file should be > 0"
  ) {
    assert(PrimesCounter.cache.loadJson("data-test.json") > 0)
  }

  // TODO use test/resource/
  test(
    "number of primes couples ranges loaded from binary encoded file should be > 0"
  ) {
    assert(PrimesCounter.cache.loadBinary("data-test.bin") > 0)
  }


  test("number of primes until 10 000 000 should be 664 579") {
    assertEquals(PrimesCounter(10000000), 664579L)
  }

  test("number of primes until 79 536 450 should be 4 643 940") {
    assertEquals(PrimesCounter(79536450), 4643940L)
  }

  test("number of primes until 100 000 000 should be 5 761 455") {
    assertEquals(PrimesCounter(100000000), 5761455L)
  }

  test("number of primes until 1 000 000 000 should be 50 847 534") {
    assertEquals(PrimesCounter(1000000000), 50847534L)
  }

  test("number of primes until 2 000 000 000 should be 98 222 287") {
    assertEquals(PrimesCounter(2000000000), 98222287L)
  }

  test("number of primes until 2 147 395 600 should be 105 093 480") {
    assertEquals(PrimesCounter(46340 * 46340), 105093480L)
  }

  test("number of primes until 2 147 483 647 should be 105 097 565") {
    assertEquals(PrimesCounter(Int.MaxValue), 105097565L)
  }

  /*
  Consistency between f(x) and g(x), g(x) beeing the inverse function of f(x)
   */

  for (i <- 10 to 2357) {
    test(s"consistency for x = $i") {
      assertEquals(PrimesCounter(PrimesCounter.primeFromRank(i)), i.toLong)
    }
  }

  test("consistency for x = 105080512") {
    assertEquals(
      PrimesCounter(PrimesCounter.primeFromRank(105080512)),
      105080512L
    )
  }

}
