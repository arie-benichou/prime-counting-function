import scala.collection.mutable

case object PrimeCounter {

  private val Singularities = Seq(
    (PrimeCouple(1, 1), 1),
    (PrimeCouple(1, 2), -2),
    (PrimeCouple(2, 3), -2))

  private val cache = new CacheManager(
    "data.json",
    Int.MinValue,
    Singularities.map(s => (s._1.asTuple, s._2)): _*)

  def updateCache(primeCouple: PrimeCouple, n: Int) = {
    cache.update(primeCouple.asTuple, n)
  }

  def clearCache() = {
    cache.clear()
    for ((pc, n) <- Singularities) updateCache(pc, n)
  }

  def regenerateCache() = {
    clearCache()
    //println(PrimeCounter(100))
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

  def multiplesOf2357(x: Int): Int = (x / 35) * 27

  def calculateMultiplesOf2357(range: Range): Int = {
    val aligned35 = range.alignEnd(35)
    val m2357 = multiplesOf2357(aligned35.end)
    if (m2357 < 0) throw new Exception("integer limit reached")
    m2357 + countMultiplesOf2357(Range(aligned35.end, range.end)) - 1
  }

  def countNumberOfPrimes(range: Range, numberOfOtherNonPrimes: Int): Int = {
    range.capacity - (calculateMultiplesOf2357(range) + numberOfOtherNonPrimes)
  }

  def memoizedCount(primeCouple: PrimeCouple): Int = {
    val memoizedValue = cache(primeCouple)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else {
      //print(s"\r $primeCouple -> ")
      updateCache(primeCouple, countOtherNonPrimes(primeCouple))
    }
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

    var lastPrimeInvolved =
      if (squareRoot - lowerBound > upperBound - squareRoot)
        upperBound
      else lowerBound

    var primeCouple = PrimeCouple(1, 1)
    var numberOfOtherNonPrimes = cache(primeCouple)

    while (primeCouple.p2 < lastPrimeInvolved) {
      primeCouple = primeCouple.next
      val memo = memoizedCount(primeCouple)
      numberOfOtherNonPrimes += memo
    }

    if (range.end > primeCouple.spanningRange.end) {
      val endingRange = Range(primeCouple.spanningRange.end + 1, range.end)
      numberOfOtherNonPrimes += countOtherNonPrimes(endingRange, lastPrimeInvolved)
    } else {
      val endingRange = Range(range.end + 1, primeCouple.spanningRange.end)
      numberOfOtherNonPrimes -= countOtherNonPrimes(endingRange, lastPrimeInvolved)
    }

    countNumberOfPrimes(range, numberOfOtherNonPrimes)

  }

  def apply(end: Int): Int = {
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    if (end < 3 * 3) PrimeUtils.getPrimesBetween(1, end).length
    else apply(Range(1, end))
  }

  // TODO parameters parsing : useCache=true, ...
  def main(args: Array[String]): Unit = {
    //loadCache()
    val n = if (args.isEmpty) 3500000 else args(0).toInt
    println(s"number of primes from 1 to $n")
    val pc = PrimeCounter(n)
    println(pc)
  }

}