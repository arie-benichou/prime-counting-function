import scala.annotation.tailrec
import scala.collection.mutable

import ch.qos.logback.classic.{Level, Logger}
import org.slf4j.LoggerFactory

import PrimeUtils._

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
    Logger.debug(Console.GREEN + s"cache has been cleared" + Console.RESET)
    for ((pc, n) <- Singularities) updateCache(pc, n)
    Logger.debug(Console.GREEN + s"cache has been initialized" + Console.RESET)
  }

  def regenerateCache() = {

    val level = LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[Logger]
      .getLevel()

    LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[Logger]
      .setLevel(Level.INFO)

    Logger.info(Console.GREEN + "regenerating cache ..." + Console.RESET)

    clearCache()
    // PrimeCounter(1000)
    PrimeCounter(Int.MaxValue)

    LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[Logger]
      .setLevel(level)

    saveCache()
  }

  def loadCache(): Int = {
    Logger.info(
      s"${Console.GREEN}Loading cache from ${cache.filename} ...${Console.RESET}"
    )
    val n = cache.load()
    Logger.info(
      s"${Console.GREEN}$n prime couples loaded from cache${Console.RESET}"
    )
    n
  }

  def saveCache() = {
    cache.save()
    Logger.info(
      s"${Console.GREEN}cache has been saved in ${cache.filename}${Console.RESET}"
    )
  }

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
      primeDivisor = findPrimeAfter(primeDivisor)
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
    Logger.debug(
      Console.YELLOW + s"number of multiples of 2, 3, 5 or 7 in $range: $sum" +
        Console.RESET
    )
    sum
  }

  def calculateNumberOfPrimes(
      range: Range,
      numberOfOtherNonPrimes: Int
  ): Int = {
    val numberOfmultiplesOf2357 = multiplesOf2357(range)
    val numberOfPrimes =
      range.capacity - (numberOfmultiplesOf2357 + numberOfOtherNonPrimes)
    Logger.debug("==========================================================")
    Logger.debug(
      Console.YELLOW +
        s"${range.capacity} -" +
        s" ($numberOfmultiplesOf2357 + " +
        s"$numberOfOtherNonPrimes) = $numberOfPrimes" +
        Console.RESET
    )
    numberOfPrimes
  }

  def memoizedCount(primeCouple: PrimeCouple): Int = {
    val memoizedValue = cache(primeCouple)
    if (memoizedValue != cache.notYetMemoizedValue) {
      Logger.debug(
        s"number of other non primes found : $memoizedValue"
      )
      memoizedValue
    } else {
      if (primeCouple.spanningRange.end < 0)
        throw new Exception("integer limit reached !")
      updateCache(primeCouple, countOtherNonPrimes(primeCouple))
    }

  }

  def next(primeCouple: PrimeCouple) =
    PrimeCouple(primeCouple.p2, findPrimeAfter(primeCouple.p2))

  def remainingRange(
      range: Range,
      primeCouple: PrimeCouple,
      numberOfOtherNonPrimes: Int,
      lastPrimeNeeded: Int
  ): Int = {

    var nonp = 0

    Logger.debug("==========================================================")
    Logger.debug(
      Console.YELLOW +
        s"total number of other non primes found in " +
        s"${range.end(primeCouple.spanningRange.end)} : " +
        s"$numberOfOtherNonPrimes" +
        Console.RESET
    )
    if (range.end - primeCouple.spanningRange.end == 0) {
      Logger.debug("==========================================================")
      Logger.debug("no range ending needed")
    } else if (range.end - primeCouple.spanningRange.end == 1) {
      if (primeCouple.p2 >= 11) {
        Logger.debug("just need to add one more")
        nonp += 1
      } else Logger.debug("no range ending needed")
    } else if (range.end > primeCouple.spanningRange.end) {
      Logger.debug("lower bound range ending")
      Logger.debug("==========================================================")
      val endingRange = Range(primeCouple.spanningRange.end + 1, range.end)
      nonp += countOtherNonPrimes(
        endingRange,
        lastPrimeNeeded
      )
    } else if (range.end < primeCouple.spanningRange.end) {
      Logger.debug("upper bound range ending")
      Logger.debug("==========================================================")
      val endingRange = Range(range.end + 1, primeCouple.spanningRange.end)
      nonp -= countOtherNonPrimes(
        endingRange,
        lastPrimeNeeded
      )
    }

    Logger.debug("==========================================================")
    Logger.debug(
      Console.YELLOW +
        s"total number of other non primes found in " +
        s"${range} : " +
        s"{$numberOfOtherNonPrimes + $nonp}" +
        Console.RESET
    )

    numberOfOtherNonPrimes + nonp
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
    Logger.debug("==========================================================")
    Logger.debug(Console.YELLOW + s"counting primes in $range" + Console.RESET)
    val squareRoot = math.floor(math.sqrt(range.end)).toInt
    Logger.debug("square root : " + squareRoot)
    val lowerBound = findPrimeBefore(squareRoot + 1)
    val upperBound = findPrimeAfter(squareRoot - 1)
    Logger.debug("lower bound for last prime : " + lowerBound)
    Logger.debug("upper bound for last prime : " + upperBound)
    val diff1 = range.end - lowerBound * lowerBound
    val diff2 = upperBound * upperBound - range.end
    val lastPrimeInvolved = if (diff2 < diff1) upperBound else lowerBound
    Logger.debug("last prime choosen : " + lastPrimeInvolved)
    Logger.debug("==========================================================")
    var primeCouple = PrimeCouple(1, 1)
    var numberOfOtherNonPrimes = cache(primeCouple)
    Logger.info(s"${Console.YELLOW}$primeCouple${Console.RESET}")
    Logger.debug(
      s"number of other non primes found : $numberOfOtherNonPrimes"
    )
    while (primeCouple.p2 < lastPrimeInvolved) {
      primeCouple = next(primeCouple)
      Logger.debug("==========================================================")
      Logger.info(Console.YELLOW + s"$primeCouple" + Console.RESET)
      val memo = memoizedCount(primeCouple)
      numberOfOtherNonPrimes += memo
    }

    val finalNumberOfOtherNonPrimes =
      remainingRange(range, primeCouple, numberOfOtherNonPrimes, lowerBound)

    Logger.debug("==========================================================")
    val numberOfPrimes =
      calculateNumberOfPrimes(range, finalNumberOfOtherNonPrimes)
    Logger.debug("==========================================================")

    numberOfPrimes
  }

  def primeFromOrder(order: Int): Int = {

    // integer implementation limit
    if (order > 105080513) throw new Exception(s"max order is 105080512")

    if (order < 0) throw new Exception(s"$order is not a valid integer !")
    if (order == 0) throw new Exception("order 0 is not defined.")

    if (order == 1) return 2
    if (order == 2) return 3
    if (order == 3) return 5
    if (order == 4) return 7

    var primeCouple = PrimeCouple(1, 1)
    var numberOfOtherNonPrimes = cache(primeCouple)
    var numberOfPrimes = 0
    var previous = (primeCouple, numberOfPrimes, numberOfOtherNonPrimes)

    while (numberOfPrimes < order) {
      previous = (primeCouple, numberOfPrimes, numberOfOtherNonPrimes)
      primeCouple = next(primeCouple)
      numberOfOtherNonPrimes += memoizedCount(primeCouple)
      numberOfPrimes = calculateNumberOfPrimes(
        primeCouple.spanningRange.start(1),
        numberOfOtherNonPrimes
      )
    }

    val last = (primeCouple, numberOfPrimes, numberOfOtherNonPrimes)

    Logger.debug(s"$previous")
    Logger.debug(s"$last")

    val (primeCouple1, numberOfPrimes1, numberOfOtherNonPrimes1) = last
    val (primeCouple2, numberOfPrimes2, numberOfOtherNonPrimes2) = previous

    val diff1 = order - numberOfPrimes2
    val diff2 = numberOfPrimes1 - order

    val interpolation =
      if (diff2 < diff1) {
        Logger.debug(s"estimation from upper bound $numberOfPrimes1 :")
        val diff = numberOfPrimes1 - order
        val ratio = math.log(primeCouple1.spanningRange.end)
        primeCouple1.spanningRange.end - (ratio * diff).toInt
      } else {
        Logger.debug(s"estimation from lower bound $numberOfPrimes2 :")
        val diff = order - numberOfPrimes2
        val ratio = math.log(primeCouple2.spanningRange.end)
        primeCouple1.spanningRange.start + (ratio * diff).toInt
      }

    val prime =
      if (isPrime(interpolation)) interpolation
      else findPrimeAfter(interpolation)

    val range = Range(1, prime)

    val finalNumberOfOtherNonPrimes =
      if (diff2 < diff1) {
        remainingRange(
          range,
          primeCouple1,
          numberOfOtherNonPrimes1,
          primeCouple1.p2
        )
      } else {
        remainingRange(
          range,
          primeCouple2,
          numberOfOtherNonPrimes2,
          primeCouple2.p2
        )
      }

    val delta =
      order - calculateNumberOfPrimes(range, finalNumberOfOtherNonPrimes)

    @inline
    @tailrec
    def after(interpolation: Int, delta: Int): Int = {
      if (delta == 0) interpolation
      else after(findPrimeAfter(interpolation), delta - 1)
    }

    @inline
    @tailrec
    def before(interpolation: Int, delta: Int): Int = {
      if (delta == 0) interpolation
      else before(findPrimeBefore(interpolation), delta + 1)
    }

    if (delta < 0)
      before(prime, delta)
    else if (delta > 0)
      after(prime, delta)
    else
      prime
  }

  def apply(end: Int): Int = {
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    if (end < 3 * 3) countPrimesUntil(end)
    else apply(Range(1, end))
  }

  // TODO tests with Long or BigInt
  def isBigPrime(n: Int) = primeFromOrder(PrimeCounter(n)) == n

  def main(args: Array[String]): Unit = {

    // return regenerateCache()

    val before = System.nanoTime;
    loadCache()
    val elapsedTime = System.nanoTime - before
    // println(elapsedTime / math.pow(10, 9))

    val n = if (args.isEmpty) 5857 else args(0).toInt

    println
    println("number           : " + n)
    println
    val order = PrimeCounter(n)
    println("order            : " + order)
    val prime = primeFromOrder(order)
    println("prime from order : " + prime)
    println
    println(s"is $n prime ?")
    println(" -> " + isBigPrime(n)) // n == prime
    println

  }

}
