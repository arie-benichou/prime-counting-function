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
    // (PrimesCouple(1, 1), 1L),

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

  private var hasUpdate = false

  private val cache = new CacheManager(
    "data.json",
    Long.MinValue,
    Singularities.map(s => (s._1.p1, s._2)): _*
  )

  @inline
  def updateCache(primesCouple: PrimesCouple, n: Long) =
    cache.update(primesCouple.p1, n)

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
    val memoizedValue = cache(primesCouple.p1)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else {
      println(primesCouple)
      hasUpdate = true
      updateCache(primesCouple, countOtherNonPrimes(primesCouple))
    }
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

  def isPrimeAlternative1(n: Long) = {
    PrimesCounter(n) - PrimesCounter(n - 1) == 1
  }

  def primeFromRank(rank: Long): Long = {

    if (rank < 0) throw new Exception(s"$rank is not a valid integer !")
    if (rank == 0) throw new Exception("rank 0 is not defined.")

    if (rank == 1) return 2
    if (rank == 2) return 3
    if (rank == 3) return 5
    if (rank == 4) return 7

    // println(rank)

    var primesCouple = PrimesCouple(1, 2)
    var numberOfOtherNonPrimes = memoizedCountOfOtherNonPrimes(primesCouple)
    var numberOfPrimesInthisRange = calculateNumberOfPrimes(
      primesCouple.spanningRange,
      numberOfOtherNonPrimes
    )
    var numberOfPrimes = numberOfPrimesInthisRange
    while (numberOfPrimes < rank) {
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
    val average = math.log(
      primesCouple.spanningRange.start + primesCouple.spanningRange.capacity / 2
    )
    val diff = numberOfPrimesInthisRange - (numberOfPrimes - rank)
    val estimate = (primesCouple.spanningRange.start + average * diff).toLong
    // println(estimate)
    val prime = if (isPrime(estimate)) estimate else findPrimeAfter(estimate)
    val partitioning =
      Partitioning.of(Range(primesCouple.spanningRange.start, prime))
    val delta = diff - PrimesCounter.evaluate(partitioning)

    @inline
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

  }

  def isPrimeAlternative2(n: Long) = {
    primeFromRank(PrimesCounter(n)) == n
  }

  def main(args: Array[String]): Unit = {

    loadCache()
    // return regenerateCache()

    val arg0 = if (args.isDefinedAt(0)) args(0).toLong else 350
    val arg1 = if (args.isDefinedAt(1)) args(1).toLong else -1

    if (arg1 == -1)
      println(s"The prime number of rank ${arg0} is ${primeFromRank(arg0)}")
    else {
      val (start, end) = (arg0, arg1)
      val partitioning = Partitioning.of(Range(start, end))
      println(
        s"Between ${arg0} and ${arg1} : ${PrimesCounter.evaluate(partitioning)} primes"
      )
    }

    if (hasUpdate) saveCache()

  }

}
