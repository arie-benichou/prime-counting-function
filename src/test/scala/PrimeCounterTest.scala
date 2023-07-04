class PrimeCounterTest extends munit.FunSuite {

  /*
  correctness tests
  */

  test(s"counting primes on 1000 ranges, without memoization, without parallelism") {
    for (n <- 1 to 1000) {
      assertEquals(PrimeCounter.apply(n, useCache = false, useParallelism = false), PrimeUtils.countPrimesUntil(n))
    }
  }

  /*
  memoization and parallelization tests
  */

  private val n = 8765432
  private val expectedResult = PrimeUtils.countPrimesUntil(n)

  test(s"counting primes until $n, without memoization, without parallelism") {
    assertEquals(PrimeCounter.apply(n, useCache = false, useParallelism = false), expectedResult)
  }

  test(s"counting primes until $n, without memoization, with parallelism") {
    assertEquals(PrimeCounter.apply(n, useCache = false, useParallelism = true), expectedResult)
  }

  test(s"counting primes until $n, with memoization, without parallelism") {
    assertEquals(PrimeCounter.apply(n, useCache = true, useParallelism = false), expectedResult)
  }

  test(s"counting primes until $n, with memoization, with parallelism") {
    assertEquals(PrimeCounter.apply(n, useCache = true, useParallelism = true), expectedResult)
  }

  /*
  Limit cases
  */

  test(s"integer limit awareness") {
    intercept[Exception] {
      PrimeCounter.apply((46340 + 1) * (46340 + 1))
    }
  }

}