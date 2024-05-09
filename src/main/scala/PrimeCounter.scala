import scala.collection.mutable

case object PrimeCounter {

  private val Singularities = Seq(
    (PrimeCouple(1, 2), 0),
    (PrimeCouple(2, 3), 0))

  private val cache = new CacheManager(
    "data-new3.json",
    Int.MinValue,
    Singularities.map(s => (s._1.asTuple, s._2)): _*)

  def updateCache(primeCouple: PrimeCouple, n: Int) = {
    cache.update(primeCouple.asTuple, n)
  }

  // TODO rename to unloadCache !!
  def clearCache() = {
    cache.clear()
    for ((pc, n) <- Singularities) updateCache(pc, n)
  }

  def regenerateCache() = {
    clearCache()
    println(PrimeCounter(Int.MaxValue))
    saveCache()
  }

  def loadCache(): Int = cache.load()

  def saveCache() = cache.save()

  def cache(primeCouple: PrimeCouple): Int = cache(primeCouple.asTuple)

  def isMultipleOf2357(n: Int): Boolean = {
    for (p <- List(2, 3, 5, 7)) if (n % p == 0) return true
    false
  }

  def countMultiplesOf2357(range: Range): Int = {
    var counter = 0
    for (n <- range.start to range.end if isMultipleOf2357(n)) counter += 1
    counter
  }

  def countOtherNonPrimes(primeCouple: PrimeCouple): Int =
    countOtherNonPrimes(primeCouple.spanningRange, primeCouple.p2)

  def countOtherNonPrimes(range: Range, maxPrimeDivisor: Int): Int = {
    val setOfOtherNonPrimes = mutable.SortedSet[Int]()
    var primeDivisor = 11
    while (primeDivisor <= maxPrimeDivisor) {
      val aligned = range % primeDivisor
      setOfOtherNonPrimes.addAll(
        for (
          onp <- aligned.start
            to aligned.end
            by primeDivisor if !isMultipleOf2357(onp)
        ) yield onp)
      primeDivisor = PrimeUtils.findPrimeAfter(primeDivisor)
    }
    setOfOtherNonPrimes.size
  }

  // TODO
  def countNumberOfPrimes(range: Range, maxPrimeDivisor: Int): Int = {
    range.capacity -
      countMultiplesOf2357(range) -
      countOtherNonPrimes(range, maxPrimeDivisor)
    return 0
  }

  def countNumberOfPrimes(primeCouple: PrimeCouple): Int =
    countNumberOfPrimes(primeCouple.spanningRange, primeCouple.p2)

  def memoizedCount(primeCouple: PrimeCouple): Int = {
    val memoizedValue = cache(primeCouple)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else updateCache(primeCouple, countOtherNonPrimes(primeCouple))
  }

  /* 
    it all starts with "anomalies" :
      1, is not prime but just an artefact
      2, is the only even number being a prime number
      3, is the only prime number away from previous prime by just one
      5, is the only prime number ending by 5
      7, is the only prime number divisible by 7
  */
  private def apply(range: Range): Int = {

    val squareRoot = math.floor(math.sqrt(range.end)).toInt
    val lowerBound = PrimeUtils.findPrimeBefore(squareRoot + 1)
    val upperBound = PrimeUtils.findPrimeAfter(squareRoot - 1)

    var lastPrimeInvolved = lowerBound

    // TODO clauses may be siplified
    if (range.end != lowerBound * lowerBound) {
      if (squareRoot - lowerBound > upperBound - squareRoot)
        lastPrimeInvolved = upperBound
    }

    lastPrimeInvolved = lowerBound

    var sum = 0
    var primeCouple = PrimeCouple(1, 1)

    while (primeCouple.p2 < lastPrimeInvolved) {
      primeCouple = primeCouple.next
      sum += memoizedCount(primeCouple)
    }

    // TODO
    if (range.end > primeCouple.spanningRange.end) {
      ///println("lower bound ending")
      val endingRange = Range(primeCouple.spanningRange.end, range.end)
      ///println(endingRange)
      val s = countOtherNonPrimes(endingRange, lastPrimeInvolved)
      ///println(s)
      sum += s
    } else if (range.end < primeCouple.spanningRange.end) {
      ///println("upper bound ending")
      ///println(primeCouple.spanningRange)
      val endingRange = Range(range.end, primeCouple.spanningRange.end)
      ///println(endingRange)
      sum -= countOtherNonPrimes(endingRange, lastPrimeInvolved)
    } else {
      ///println("perfect range ending")
    }

    // TODO extract to countPrimes method
    def f(x: Int): Int = (x / 35) * 27
    val alignedRangeEndOn35 = range.alignEnd(35)
    val m2357 = f(alignedRangeEndOn35.end)
    if (m2357 < 0) throw new Exception("integer limit reached")
    val r = Range(alignedRangeEndOn35.end, range.end)
    val mm = countMultiplesOf2357(r)
    range.capacity - (sum + (m2357 + mm - 1)) + 4 - 1

  }

  def apply(end: Int): Int = {
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    if (end < 3 * 3) PrimeUtils.getPrimesBetween(1, end).length
    else apply(Range(1, end))
  }

  // TODO add unit test
  def main(args: Array[String]): Unit = {
    //regenerateCache()
    loadCache()
    val n = 79536450
    val pc = PrimeCounter(n)
    println("" + n + " -> " + pc)
    //val expected = PrimeUtils.countPrimesUntil(n)
    //println(expected)
    //if (pc != expected) throw new Exception(s"expected $expected instead of $pc")
  }

}