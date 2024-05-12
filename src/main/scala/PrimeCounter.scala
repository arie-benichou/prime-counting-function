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

  private val Logger =
    com.typesafe.scalalogging.Logger(getClass)

  def updateCache(primeCouple: PrimeCouple, n: Int) = {
    Logger.debug(
      Console.GREEN + s"cache updated : $primeCouple -> $n" + Console.RESET
    )
    cache.update(primeCouple.asTuple, n)
  }

  def clearCache() = {
    cache.clear()
    Logger.debug(Console.GREEN + s"cache cleared" + Console.RESET)
    for ((pc, n) <- Singularities) updateCache(pc, n)
  }

  def regenerateCache() = {
    clearCache()
    Logger.debug(
      Console.BLINK + Console.GREEN + "regenerating cache ..." + Console.RESET
    )
    // PrimeCounter(10000000)
    PrimeCounter(Int.MaxValue)
    Logger.debug("\n\u001b[%dA\u001b[2K".format(2))
    saveCache()
  }

  def loadCache(): Int = cache.load()

  def saveCache() = cache.save()

  def cache(primeCouple: PrimeCouple): Int = cache(primeCouple.asTuple)

  def isMultipleOf2357(n: Int): Boolean = {
    for (p <- List(2, 3, 5, 7)) if (n % p == 0) return true
    false
  }

  def countOtherNonPrimes(primeCouple: PrimeCouple): Int = {
    countOtherNonPrimes(primeCouple.spanningRange, primeCouple.p1)
  }

  def countOtherNonPrimes(range: Range, maxPrimeDivisor: Int): Int = {
    Logger.debug(s"$range : max prime divisor needed : " + maxPrimeDivisor)
    Logger.debug("==========================================================")
    val setOfOtherNonPrimes = mutable.SortedSet[Int]()
    var primeDivisor = 11
    while (primeDivisor <= maxPrimeDivisor) {
      Logger.debug("checking multiples of : " + primeDivisor)
      val aligned = range % primeDivisor
      for (
        onp <- aligned.start
          to aligned.end
          by primeDivisor
        if !isMultipleOf2357(onp)
      ) {
        Logger.debug(s"$onp")
        setOfOtherNonPrimes.add(onp)
      }
      primeDivisor = PrimeUtils.findPrimeAfter(primeDivisor)
    }
    Logger.debug(
      "number of other non primes found : " + setOfOtherNonPrimes.size
    )
    setOfOtherNonPrimes.size
  }

  def countMultiplesOf2357(range: Range): Int = {
    var counter = 0
    for (n <- range.start to range.end if isMultipleOf2357(n)) counter += 1
    counter
  }

  def calculateMultiplesOf2357(x: Int): Int = (x / 35) * 27

  def multiplesOf2357(range: Range): Int = {
    val aligned35 = range.alignEnd(35)
    Logger.debug(s"$range aligned by 35 at end : $aligned35")
    val multiplesOf2357From1 = calculateMultiplesOf2357(aligned35.end)
    Logger.debug(
      s"number of multiples of 2, 3, 5 or 7 in $aligned35 : $multiplesOf2357From1"
    )
    if (multiplesOf2357From1 < 0) throw new Exception("integer limit reached")
    val endingRange = Range(aligned35.end + 1, range.end)
    Logger.debug(s"ending range : $endingRange")
    val multiplesOf2357InEndingRange = countMultiplesOf2357(endingRange)
    Logger.debug(
      s"number of multiples of 2, 3, 5 or 7 in $endingRange : " +
        multiplesOf2357InEndingRange
    )
    val sum = multiplesOf2357From1 + multiplesOf2357InEndingRange
    Logger.debug("==========================================================")
    Logger.debug(s"number of multiples of 2, 3, 5 or 7 in $range: $sum")
    sum
  }

  def countNumberOfPrimes(range: Range, numberOfOtherNonPrimes: Int): Int = {
    val numberOfmultiplesOf2357 = multiplesOf2357(range)
    val numberOfPrimes =
      range.capacity - (numberOfmultiplesOf2357 + numberOfOtherNonPrimes)
    Logger.debug("==========================================================")
    Logger.debug(
      s"${range.capacity} -" +
        s" ($numberOfmultiplesOf2357 + " +
        s"$numberOfOtherNonPrimes) = $numberOfPrimes"
    )
    numberOfPrimes
  }

  def memoizedCount(primeCouple: PrimeCouple): Int = {
    val memoizedValue = cache(primeCouple)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else updateCache(primeCouple, countOtherNonPrimes(primeCouple))
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

    Logger.debug("==========================================================")
    Logger.debug(Console.YELLOW + s"$range" + Console.RESET)

    val squareRoot = math.floor(math.sqrt(range.end)).toInt
    Logger.debug("square root : " + squareRoot)

    val lowerBound = PrimeUtils.findPrimeBefore(squareRoot + 1)
    val upperBound = PrimeUtils.findPrimeAfter(squareRoot - 1)
    Logger.debug("lower bound for last prime : " + lowerBound)
    Logger.debug("upper bound for last prime : " + upperBound)

    val diff1 = range.end - lowerBound * lowerBound

    val diff2 = upperBound * upperBound - range.end
    val lastPrimeInvolved = if (diff2 < diff1) upperBound else lowerBound

    Logger.debug("last prime choosen : " + lastPrimeInvolved)

    // TODO traces
    var primeCouple = PrimeCouple(1, 1)
    var numberOfOtherNonPrimes = cache(primeCouple)

    Logger.debug("==========================================================")
    Logger.info(Console.YELLOW + s"$primeCouple" + Console.RESET)
    Logger.debug(
      s"number of other non primes found in $primeCouple : $numberOfOtherNonPrimes"
    )
    while (primeCouple.p2 < lastPrimeInvolved) {
      primeCouple = next(primeCouple)
      Logger.debug("==========================================================")
      Logger.info(Console.YELLOW + s"$primeCouple" + Console.RESET)
      val memo = memoizedCount(primeCouple)
      Logger.debug(
        s"number of other non primes found in $primeCouple : $memo"
      )
      numberOfOtherNonPrimes += memo
    }

    Logger.debug("==========================================================")
    Logger.debug(
      s"total number of other non primes found : $numberOfOtherNonPrimes"
    )

    if (range.end - primeCouple.spanningRange.end == 0) {
      Logger.debug("no range ending needed")
    } else if (range.end - primeCouple.spanningRange.end == 1) {
      if (lastPrimeInvolved >= 11) {
        Logger.debug("just add one more")
        numberOfOtherNonPrimes += 1
      } else {
        Logger.debug("no range ending needed")
      }

    } else if (range.end > primeCouple.spanningRange.end) {
      Logger.debug("lower bound range ending")
      Logger.debug("==========================================================")
      val endingRange = Range(primeCouple.spanningRange.end + 1, range.end)
      numberOfOtherNonPrimes += countOtherNonPrimes(
        endingRange,
        lowerBound
      )
    } else if (range.end < primeCouple.spanningRange.end) {
      Logger.debug("upper bound range ending")
      Logger.debug("==========================================================")
      val endingRange = Range(range.end + 1, primeCouple.spanningRange.end)
      numberOfOtherNonPrimes -= countOtherNonPrimes(
        endingRange,
        lowerBound
      )
    }

    Logger.debug("==========================================================")
    val numberOfPrimes = countNumberOfPrimes(range, numberOfOtherNonPrimes)
    Logger.debug("==========================================================")

    numberOfPrimes

  }

  def apply(end: Int): Int = {
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    if (end < 3 * 3) PrimeUtils.countPrimesUntil(end)
    else apply(Range(1, end))
  }

  // TODO parameters parsing : useCache=true, ...
  def main(args: Array[String]): Unit = {

    // return regenerateCache()

    val n = if (args.isEmpty) 289 else args(0).toInt
    // loadCache()

    val pc = PrimeCounter(n)
    println(s"number of primes from 1 to $n : $pc")
    val expected = PrimeUtils.countPrimesUntil(n)
    if (expected != pc) throw new Exception("expected : " + expected)

  }

}
