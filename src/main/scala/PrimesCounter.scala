import scala.collection.mutable
import PrimesUtils._
import Primes2357._

object PrimesCounter {

  private val Singularities = Seq(
    (PrimesCouple(1, 1), 1L),
    (PrimesCouple(1, 2), -2L),
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
        if (middle.p1 == 1) 1 else findPrimeAfter(middle.p1)
      )
      var numberOfOtherNonPrimes = memoizedCountOfOtherNonPrimes(primesCouple)
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

  def main(args: Array[String]): Unit = {

    // loadCache()
    // return regenerateCache()

    val (start, end) = (args(0).toLong, args(1).toLong)
    val partitioning = Partitioning.of(Range(start, end))
    println(partitioning.traces)
    println(PrimesCounter.evaluate(partitioning))

    println(getPrimesBetween(start, end).size) // TODO count

  }

}
