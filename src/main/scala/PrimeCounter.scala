import scala.collection.mutable

case object PrimeCounter {

  private val Singularities = Seq(
    (PrimeCouple(1, 1), 1),
    (PrimeCouple(1, 2), -2),
    (PrimeCouple(2, 3), -2)
  )

  private val cache = new CacheManager(
    "data.json",
    Int.MinValue,
    Singularities.map(s => (s._1.asTuple, s._2)): _*
  )

  def updateCache(primeCouple: PrimeCouple, n: Int) = {
    cache.update(primeCouple.asTuple, n)
  }

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

  def countOtherNonPrimes(primeCouple: PrimeCouple): Int =
    countOtherNonPrimes(primeCouple.spanningRange, primeCouple.p1)

  def countOtherNonPrimes(range: Range, maxPrimeDivisor: Int): Int = {
    val setOfOtherNonPrimes = mutable.SortedSet[Int]()
    var primeDivisor = 11
    while (primeDivisor <= maxPrimeDivisor) {
      val aligned = range % primeDivisor
      for (
        onp <- aligned.start
          to aligned.end
          by primeDivisor if !isMultipleOf2357(onp)
      ) setOfOtherNonPrimes.addOne(onp)
      primeDivisor = PrimeUtils.findPrimeAfter(primeDivisor)
    }
    setOfOtherNonPrimes.size
  }

  def countMultiplesOf2357(range: Range): Int = {
    var counter = 0
    for (n <- range.start to range.end if isMultipleOf2357(n)) counter += 1
    counter
  }

  def calculateMultiplesOf2357(x: Int): Int = (x / 35) * 27

  // TODO rename
  def multiplesOf2357(range: Range): Int = {
    val aligned35 = range.alignEnd(35)
    val m2357 = calculateMultiplesOf2357(aligned35.end)
    if (m2357 < 0) throw new Exception("integer limit reached")
    m2357 + countMultiplesOf2357(Range(aligned35.end + 1, range.end))
  }

  def countNumberOfPrimes(range: Range, numberOfOtherNonPrimes: Int): Int = {
    range.capacity - (multiplesOf2357(range) + numberOfOtherNonPrimes)
  }

  def memoizedCount(primeCouple: PrimeCouple): Int = {
    val memoizedValue = cache(primeCouple)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else {
      // println(primeCouple)
      updateCache(primeCouple, countOtherNonPrimes(primeCouple))
    }
  }

  def next(primeCouple: PrimeCouple) =
    PrimeCouple(primeCouple.p2, PrimeUtils.findPrimeAfter(primeCouple.p2))

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

    val diff1 = squareRoot - lowerBound
    val diff2 = upperBound - squareRoot

    var lastPrimeInvolved =
      if (diff2 < diff1) upperBound
      else if (diff1 < diff2) lowerBound
      else squareRoot

    var primeCouple = PrimeCouple(1, 1)
    var numberOfOtherNonPrimes = cache(primeCouple)

    while (primeCouple.p2 < lastPrimeInvolved) {
      primeCouple = next(primeCouple)
      val memo = memoizedCount(primeCouple)
      numberOfOtherNonPrimes += memo
    }

    if (range.end > primeCouple.spanningRange.end + 1) {
      val endingRange = Range(primeCouple.spanningRange.end + 1, range.end)
      numberOfOtherNonPrimes += countOtherNonPrimes(
        endingRange,
        lastPrimeInvolved
      )
    } else if (range.end < primeCouple.spanningRange.end + 1) {
      val endingRange = Range(range.end + 1, primeCouple.spanningRange.end)
      numberOfOtherNonPrimes -= countOtherNonPrimes(
        endingRange,
        lastPrimeInvolved
      )
    } else if (lastPrimeInvolved >= 11) numberOfOtherNonPrimes += 1

    countNumberOfPrimes(range, numberOfOtherNonPrimes)

  }

  def apply(end: Int): Int = {
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    if (end < 3 * 3) PrimeUtils.countPrimesUntil(end)
    else apply(Range(1, end))
  }

  // TODO parameters parsing : useCache=true, ...
  def main(args: Array[String]): Unit = {
    
    // return regenerateCache()

    val end = if (args.isEmpty) 9999 else args(0).toInt
    
    loadCache()
    
    for (n <- 0 to end) {
      val pc = PrimeCounter(n)
      println(s"number of primes from 1 to $n : $pc")
      val expected = PrimeUtils.countPrimesUntil(n)
      if (expected != pc) throw new Exception("inconsistent result !")
    }

  }

}
