class Primes2357Test extends munit.FunSuite {

  test("mapping for fast counting") {
    assertEquals(
      Primes2357.MapForFastCounting,
      Map(
        "+" -> List(2, 3, 5, 7, 30, 42, 70, 105),
        "-" -> List(6, 10, 14, 15, 21, 35, 210)
      )
    )
  }

  test("count multiples of {2, 3, 5, 7} from 1 to 35") {
    assertEquals(
      Primes2357.numberOfMultiplesIn(Range(1, 35)),
      27L
    )
  }

  for (n <- 34 to 2 by -1) {
    test(s"count multiples of {2, 3, 5, 7} from 1 to $n") {
      assertEquals(
        Primes2357.numberOfMultiplesIn(Range(1, n)),
        PrimesUtils.countMultiplesOf2357(1, n)
      )
    }
  }

  test("count multiples of {2, 3, 5, 7} from 1 to 1") {
    assertEquals(
      Primes2357.numberOfMultiplesIn(Range(1, 1)),
      0L
    )
  }

  test("count multiples of {2, 3, 5, 7} from 36 to 211") {
    for (n <- 36 to 211) {
      val range = Range(1, n)
      assertEquals(
        Primes2357.numberOfMultiplesIn(range),
        PrimesUtils.countMultiplesOf2357(1, n)
      )
    }
  }

  for (n <- 1 to 211) {
    test(s"count multiples of {2, 3, 5, 7} in singleton {$n} : [$n, $n]") {
      assertEquals(
        Primes2357.numberOfMultiplesIn(Range(n, n)),
        PrimesUtils.countMultiplesOf2357(n, n)
      )
    }
  }

  test(s"mapping for multiples of {2, 3, 5, 7}") {
    assertEquals(Primes2357.MapForMultiplesOf2357.size, 210)
    for (n <- 0 to 210 * 11 + 1) {
      assertEquals(
        Primes2357.divides(n),
        PrimesUtils.isMultipleOf2357(n)
      )
    }
  }

}
