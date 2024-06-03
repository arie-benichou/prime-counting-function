import scala.annotation.tailrec
import scala.collection.mutable
import PrimesUtils._
import Primes2357._

object PrimesCounter {

  private val Singularities = Seq(
    /* Between (1 x 1) and (1 x 1) there is one number that is :
     * - neither a multiple 2, 3, 5 or 7
     * - neither a prime number
     * Since it's 1 and 1 is neither a multiple of
     * 11, 13, 17, 19 and so on
     * we need to handle this special case or "singularity"
     * by treating 1 as an "other non prime", like we would do for
     * any multiples of 11, 13, 17, 19 and so on
     * This singularity is a symbolic range that is never used in computation
     * Its sole purpose is to explain the next concrete singularity
     */
    (PrimesCouple(1, 1), 1L),

    /* Between (1 x 1) and (2 x 2) there are two numbers that are :
     *  - multiples of 2, 3, 5 or 7
     *  - and also primes : {2, 3}
     * Hence, we would need to handle this special case or "singularity"
     * by assigning to this range the value of -2.
     * However, since this range includes 1,
     * we need to assign to this range the value of (-2 + 1), hence : -1
     */
    (PrimesCouple(1, 2), -1L),

    /* Between (2 x 2) and (3 x 3) there are two numbers that are :
     *  - multiples of 2, 3, 5 or 7
     *  - and also primes : {5, 7}
     * Hence, we need to handle this special case or "singularity"
     * by assigning to this range the value of -2
     */
    (PrimesCouple(2, 3), -2L)
  )

  private val cache = new CacheManager(
    "data.json",
    Long.MinValue,
    Singularities.map(s => (s._1.asTuple, s._2)): _*
  )

  def updateCache(primesCouple: PrimesCouple, n: Long) =
    cache.update(primesCouple.asTuple, n)

  def clearCache() = {
    cache.clear()
    for ((pc, n) <- Singularities) updateCache(pc, n)
  }

  def saveCache() = {
    cache.save()
    println(
      s"${Console.GREEN}cache is being saved in ${cache.filename}${Console.RESET}"
    )
  }

  def augmentCache(end: Long = Int.MaxValue): Unit = {
    println(Console.GREEN + "augmenting cache ..." + Console.RESET)
    println(PrimesCounter(end))
    saveCache()
  }

  def regenerateCache(end: Long = Int.MaxValue): Unit = {
    println(Console.GREEN + "regenerating cache ..." + Console.RESET)
    clearCache()
    println(PrimesCounter(end))
    saveCache()
  }

  def loadCache(): Long = {
    println(
      s"${Console.GREEN}Loading cache from ${cache.filename}${Console.RESET}"
    )
    val n = cache.load()
    println(
      s"${Console.GREEN}$n prime couples loaded from cache${Console.RESET}"
    )
    n
  }

  @inline
  def countOtherNonPrimes(range: Range, maxPrimeDivisor: Long): Long = {
    val setOfOtherNonPrimes = mutable.Set[Long]()
    var primeDivisor = 11L // TODO extract constant
    while (primeDivisor <= maxPrimeDivisor) {
      val (start, end) = range.alignedEdgesOn(primeDivisor)
      for (onp <- start to end by primeDivisor if Primes2357.doesNotdivide(onp))
        setOfOtherNonPrimes.addOne(onp)
      primeDivisor = findPrimeAfter(primeDivisor)
    }
    setOfOtherNonPrimes.size
  }

  @inline
  def countOtherNonPrimes(primesCouple: PrimesCouple): Long =
    countOtherNonPrimes(primesCouple.spanningRange, primesCouple.p1)

  @inline
  def calculateNumberOfPrimes(
      range: Range,
      numberOfOtherNonPrimes: Long
  ): Long = range.capacity - (multiplesOf2357(range) + numberOfOtherNonPrimes)

  @inline
  def memoizedCountOfOtherNonPrimes(primesCouple: PrimesCouple): Long = {
    val memoizedValue = cache(primesCouple.asTuple)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else updateCache(primesCouple, countOtherNonPrimes(primesCouple))
  }

  def evaluate(middle: Partitioning.Middle): Long = {
    if (middle.isSingleton) 0
    else {
      var primesCouple = PrimesCouple(
        middle.p1,
        // if (middle.p1 == 1) 1 else
        findPrimeAfter(middle.p1)
      )
      var numberOfOtherNonPrimes = memoizedCountOfOtherNonPrimes(primesCouple)
      // println(primesCouple)
      while (primesCouple.p2 < middle.p2) {
        primesCouple =
          PrimesCouple(primesCouple.p2, findPrimeAfter(primesCouple.p2))
        // println(primesCouple)
        numberOfOtherNonPrimes += memoizedCountOfOtherNonPrimes(primesCouple)
      }
      // TODO extract constant
      if (primesCouple.p2 >= 11) numberOfOtherNonPrimes += 1
      calculateNumberOfPrimes(middle, numberOfOtherNonPrimes)
    }
  }

  def evaluate(range: Range, lastPrimeInvolved: Long): Long = {
    if (Partitioning.RangeSingularities.isDefinedAt(range))
      Partitioning.RangeSingularities(range)
    else if (range.isSingleton)
      if (PrimesUtils.isPrime(range.start)) 1 else 0
    else
      calculateNumberOfPrimes(
        range,
        countOtherNonPrimes(range, lastPrimeInvolved)
      )
  }

  def evaluate(side: Partitioning.Side): Long =
    if (side.isNull) 0
    else side.polarity * evaluate(side.unsignedRange, side.prime)

  def evaluate(part: Partitioning.Partitions): Long = {
    if (part.isWorthy)
      evaluate(part.left) + evaluate(part.middle) + evaluate(part.right)
    else
      evaluate(part.initialRange, part.middle.p2)
  }

  def apply(start: Long, end: Long): Long = {
    if (start < 0) throw new Exception(s"$start is not a valid integer !")
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    if (start > end)
      throw new Exception(s"[$start ; $end] is not a valid range !")
    evaluate(Partitioning.of(Range(start, end)))
  }

  def apply(end: Long): Long = apply(1, end)

  def primeFromOrder(order: Long): Long = {

    if (order < 0) throw new Exception(s"$order is not a valid integer !")
    if (order == 0) throw new Exception("order 0 is not defined.")

    if (order == 1) return 2
    if (order == 2) return 3
    if (order == 3) return 5
    if (order == 4) return 7

    println(order)

    var primesCouple = PrimesCouple(1, 2)
    var numberOfOtherNonPrimes = memoizedCountOfOtherNonPrimes(primesCouple)

    var numberOfPrimesInthisRange = calculateNumberOfPrimes(
      primesCouple.spanningRange,
      numberOfOtherNonPrimes
    )

    var numberOfPrimes = numberOfPrimesInthisRange

    while (numberOfPrimes < order) {

      primesCouple =
        PrimesCouple(primesCouple.p2, findPrimeAfter(primesCouple.p2))

      // println(primesCouple)

      val memo = memoizedCountOfOtherNonPrimes(primesCouple)
      numberOfOtherNonPrimes += memo

      numberOfPrimesInthisRange = calculateNumberOfPrimes(
        primesCouple.spanningRange,
        memo
      )

      numberOfPrimes += numberOfPrimesInthisRange

    }

    // println(numberOfPrimes)

    // println(primesCouple)
    // println(numberOfPrimesInthisRange)

    // val average1 =
    //   primesCouple.spanningRange.capacity / numberOfPrimesInthisRange
    // println("average1 :" + average1)

    val average = math.log(
      primesCouple.spanningRange.start + primesCouple.spanningRange.capacity / 2
    )
    val diff = numberOfPrimesInthisRange - (numberOfPrimes - order)
    val estimate = (primesCouple.spanningRange.start + average * diff).toLong
    //println(estimate)

    val prime = if (isPrime(estimate)) estimate else findPrimeAfter(estimate)

    val partitioning =
      Partitioning.of(Range(primesCouple.spanningRange.start, prime))
    //println(partitioning.traces)
    val eval = PrimesCounter.evaluate(partitioning)
    //println("eval : " + eval)

    val delta = diff - eval
    //println(delta)

    def findPrime(prime: Long, direction: Long): Long = {
      if (direction > 0) findPrimeAfter(prime)
      else findPrimeBefore(prime)
    }

    @inline
    @tailrec
    def after(prime: Long, delta: Long): Long = {
      if (delta == 0) prime
      else after(findPrime(prime, 1), delta - 1)
    }

    @inline
    @tailrec
    def before(prime: Long, delta: Long): Long = {
      if (delta == 0) prime
      else before(findPrime(prime, -1), delta + 1)
    }

    if (delta < 0) before(prime, delta)
    else if (delta > 0) after(prime, delta)
    else prime

    /*

    val (primesCouple1, numberOfPrimes1, numberOfOtherNonPrimes1) = last
    val (primesCouple2, numberOfPrimes2, numberOfOtherNonPrimes2) = previous

    val diff1 = order - numberOfPrimes2
    val diff2 = numberOfPrimes1 - order

    val estimate =
      if (diff2 < diff1) {
        // println(s"estimation from upper bound $numberOfPrimes1 :")
        val diff = numberOfPrimes1 - order
        val ratio = math.log(primesCouple1.spanningRange.end)
        primesCouple1.spanningRange.end - (ratio * diff).toLong
      } else {
        // println(s"estimation from lower bound $numberOfPrimes2 :")
        val diff = order - numberOfPrimes2
        val ratio = math.log(primesCouple2.spanningRange.end)
        primesCouple1.spanningRange.start + (ratio * diff).toLong
      }

    def findPrime(prime: Long, direction: Long): Long = {
      if (direction > 0) findPrimeAfter(prime)
      else findPrimeBefore(prime)
    }

    val prime =
      if (estimate >= primesCouple1.spanningRange.end) {
        // println("upper bound overflow")
        findPrime(primesCouple1.spanningRange.end, -1)
      } else if (estimate <= primesCouple2.spanningRange.start) {
        // println("lower bound overflow")
        findPrime(primesCouple2.spanningRange.start, 1)
      } else {
        if (isPrime(estimate)) estimate
        else {
          val d1 = primesCouple1.spanningRange.end - estimate
          val d2 = estimate - primesCouple1.spanningRange.start
          val direction = if (d1 < d2) -1 else 1
          findPrime(estimate, direction)
        }
      }

    val range = Range(1, prime)

    val finalNumberOfOtherNonPrimes =
      if (diff2 < diff1) {
        PrimeCounter.remainingRange(
          range,
          primesCouple1,
          numberOfOtherNonPrimes1,
          primesCouple1.p2
        )
      } else {
        PrimeCounter.remainingRange(
          range,
          primesCouple2,
          numberOfOtherNonPrimes2,
          primesCouple2.p2
        )
      }

    val delta = order -
      calculateNumberOfPrimes(range, finalNumberOfOtherNonPrimes)

    @inline
    @tailrec
    def after(prime: Long, delta: Long): Long = {
      if (delta == 0) prime
      else after(findPrime(prime, 1), delta - 1)
    }

    @inline
    @tailrec
    def before(prime: Long, delta: Long): Long = {
      if (delta == 0) prime
      else before(findPrime(prime, -1), delta + 1)
    }

    if (delta < 0) before(prime, delta)
    else if (delta > 0) after(prime, delta)
    else prime


  }
     */
  }

  def main(args: Array[String]): Unit = {

    loadCache()
    // return regenerateCache()

    val prime = primeFromOrder(args(0).toLong) // TODO rename into primeFromRank

    println(prime)

    val (start, end) = (1, prime) // (args(0).toLong, args(1).toLong)
    val partitioning = Partitioning.of(Range(start, end))
    // println(partitioning.traces)
    println(PrimesCounter.evaluate(partitioning))

    // println(getPrimesBetween(start, end).size) // TODO count

  }

}
