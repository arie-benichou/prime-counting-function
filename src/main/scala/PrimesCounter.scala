import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.annotation.tailrec

import PrimesUtils._
import Partitioning._
import DistinctComposites._

object PrimesCounter {

  val AvailableProcessors = Runtime.getRuntime.availableProcessors

  val MaxRangeCapacitySegment = Int.MaxValue

  // TODO : rename Singularities as MiddleSingularities
  // TODO : rename RangeSingularities as SideSingularities
  // TODO : RangeSingularities generation is PrimesCounter concern
  //        not Partitioning concern
  val Singularities = Seq(
    (PrimesCouple(1, 2), 2L),
    (PrimesCouple(2, 3), 2L)
  )

  val cache = new CacheManager(
    Long.MinValue,
    Singularities.map(s => (s._1.p1, s._2)): _*
  )

  val FifthPrime = 11L

  private var hasUpdate = false

  def countOtherNonPrimes(
      range: Range,
      maxPrimeDivisor: Long
  ): Long = {
    val distinctComposites = new DistinctComposites(range.capacity.toInt)
    @inline
    def mark(x: Long) = {
      val bitRank = (x - range.start).toInt
      distinctComposites synchronized { distinctComposites.set(bitRank) }
    }
    case class Task(range: Range, prime: Long) extends Callable[Unit] {
      private val (start, end) = range.alignedEdgesOn(prime)
      def call = for (
        x <- start to end by prime
        if Primes2357.doesNotdivide(x)
      ) mark(x)
    }
    val executorService = Executors.newWorkStealingPool(AvailableProcessors)
    var primeDivisor = maxPrimeDivisor
    while (primeDivisor >= FifthPrime) {
      executorService.submit(new Task(range, primeDivisor))
      primeDivisor = findPrimeBefore(primeDivisor)
    }
    executorService.shutdown()
    executorService.awaitTermination(1, TimeUnit.DAYS)
    distinctComposites.count
  }

  @inline
  private def applySegmentation(
      range: Range,
      maxPrimeDivisor: Long,
      isMiddleRange: Boolean
  ): Long = {
    val capacity = range.capacity
    val q = capacity / MaxRangeCapacitySegment
    if (q != 0) {
      print(Console.GREEN + "segmentation of " + Console.RESET)
      if (!isMiddleRange)
        println(
          s"$range -> ${range.capacity}"
        )
    }
    val r = capacity % MaxRangeCapacitySegment
    var (start, end) = (range.start, range.end)
    var sum = 0L
    for (i <- 1 to q.toInt) {
      end = start + MaxRangeCapacitySegment - 1
      sum += countOtherNonPrimes(Range(start, end), maxPrimeDivisor)
      start = end + 1
    }
    if (r != 0)
      sum += countOtherNonPrimes(Range(start, start + r - 1), maxPrimeDivisor)
    sum
  }

  @inline
  private def countOtherNonPrimes(
      range: Range,
      maxPrimeDivisor: Long,
      isMiddleRange: Boolean
  ): Long =
    applySegmentation(range, maxPrimeDivisor, isMiddleRange)

  @inline
  def countOtherNonPrimes(pc: PrimesCouple): Long =
    countOtherNonPrimes(pc.spanningRange, pc.p1, true)

  @inline
  def calculateNumberOfPrimes(
      range: Range,
      numberOfOtherNonPrimes: Long
  ) =
    range.capacity - (
      Primes2357.numberOfMultiplesIn(range) + numberOfOtherNonPrimes
    )

  @inline
  private def updateCache(primesCouple: PrimesCouple, n: Long): Long = {
    hasUpdate = true
    cache.update(primesCouple.p1, n)
  }

  private def memoizedNumberOfPrimes(primesCouple: PrimesCouple): Long = {
    val memoizedValue = cache(primesCouple.p1)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else {
      val numberOfOtherNonPrimes = countOtherNonPrimes(primesCouple)
      val numberOfPrimes = calculateNumberOfPrimes(
        primesCouple.spanningRange,
        numberOfOtherNonPrimes
      )
      println(
        s"$primesCouple -> ${primesCouple.spanningRange.capacity} -> $numberOfOtherNonPrimes -> $numberOfPrimes"
      )
      updateCache(primesCouple, numberOfPrimes)
    }
  }

  /*
   * TODO grid computing
   * TODO pagination when number of cache entries > Int.MaxValue
   */
  private def evaluate(middle: Middle): Long = {
    if (middle.isSingleton) 0
    else {
      var primesCouple = PrimesCouple(middle.p1, findPrimeAfter(middle.p1))
      var numberOfPrimes = memoizedNumberOfPrimes(primesCouple)
      while (primesCouple.p2 < middle.p2) {
        primesCouple = PrimesCouple(
          primesCouple.p2,
          findPrimeAfter(primesCouple.p2)
        )
        numberOfPrimes += memoizedNumberOfPrimes(primesCouple)
      }
      numberOfPrimes
    }
  }

  private def evaluate(range: Range, lastPrimeInvolved: Long): Long = {
    if (RangeSingularities.isDefinedAt(range)) RangeSingularities(range)
    else if (range.isSingleton) if (isPrime(range.start)) 1 else 0
    else
      calculateNumberOfPrimes(
        range,
        countOtherNonPrimes(range, lastPrimeInvolved, false)
      )
  }

  private def evaluate(side: Side): Long =
    if (side.isNull) 0
    else side.polarity * evaluate(side.unsignedRange, side.prime)

  def evaluate(part: Partitions): Long = {
    if (part.isWorthy)
      evaluate(part.left) + evaluate(part.middle) + evaluate(part.right)
    else
      evaluate(part.initialRange, part.middle.p2)
  }

  def apply(start: Long, end: Long): Long = {
    require(start >= 0, s"$start is not a valid integer !")
    require(end >= 0, s"$end is not a valid integer !")
    require(start <= end, s"[$start ; $end] is not a valid range !")
    evaluate(Partitioning.of(Range(start, end)))
  }

  // TODO ! prime from rank with apply(x)
  def apply(end: Long): Long = apply(1, end)

  // TODO ? use Gauss Li(x) as a better estimate
  def primeFromRank(rank: Long): Long = {
    require(rank > 0, s"$rank is not a valid integer !")
    require(rank != 0, "rank 0 is not defined.") // could return 1 ?

    if (rank == 1) return 2
    if (rank == 2) return 3
    if (rank == 3) return 5
    if (rank == 4) return 7

    @inline
    def findPrime(prime: Long, direction: Long): Long =
      if (direction > 0) findPrimeAfter(prime) else findPrimeBefore(prime)

    @tailrec
    def after(prime: Long, delta: Long): Long = {
      if (delta == 0) prime
      else after(findPrime(prime, 1), delta - 1)
    }

    @tailrec
    def before(prime: Long, delta: Long): Long = {
      if (delta == 0) prime
      else before(findPrime(prime, -1), delta + 1)
    }

    var primesCouple = PrimesCouple(1, 2)
    var numberOfPrimesInthisRange = memoizedNumberOfPrimes(primesCouple)
    var numberOfPrimes = numberOfPrimesInthisRange

    while (numberOfPrimes < rank) {
      primesCouple = PrimesCouple(
        primesCouple.p2,
        findPrimeAfter(primesCouple.p2)
      )
      numberOfPrimesInthisRange = memoizedNumberOfPrimes(primesCouple)
      numberOfPrimes += numberOfPrimesInthisRange
    }

    val range = primesCouple.spanningRange
    val density = (1.0 * range.capacity) / numberOfPrimesInthisRange
    val diff = numberOfPrimesInthisRange - (numberOfPrimes - rank)
    val tmp = (1.0 * range.start + density * 1.0 * diff).toLong
    val estimate = if (tmp % 2 == 0) tmp - 1 else tmp
    val prime = if (isPrime(estimate)) estimate else findPrimeAfter(estimate)
    val delta = diff - evaluate(Partitioning.of(Range(range.start, prime)))
    if (delta < 0) before(prime, delta)
    else if (delta > 0) after(prime, delta)
    else prime
  }

  // TODO R&D with this alternative
  def isPrimeAlternative1(n: Long) =
    PrimesCounter(n) - PrimesCounter(n - 1) == 1

  // TODO R&D with this alternative
  def isPrimeAlternative2(n: Long) =
    primeFromRank(PrimesCounter(n)) == n

  def main(args: Array[String]): Unit = {

    val inputCacheFileName = "data.bin"
    val outputCacheFileName = inputCacheFileName

    cache.loadBinary(inputCacheFileName)

    val arg0 = if (args.isDefinedAt(0)) args(0).toLong else 350
    val arg1 = if (args.isDefinedAt(1)) args(1).toLong else -1

    if (arg1 == -1) {
      val prime = primeFromRank(arg0)
      println(s"The prime number of rank ${arg0} is ${prime}")
      // val partitioning = Partitioning.of(Range(1, prime))
      // println(
      //   s"Between ${1} and ${prime} : ${evaluate(partitioning)} primes"
      // )
    } else {
      val (start, end) = (arg0, arg1)
      val partitioning = Partitioning.of(Range(start, end))
      val rank = evaluate(partitioning)
      println(
        s"Between ${start} and ${end} : ${rank} primes"
      )
      // val prime = primeFromRank(rank)
      // println(s"The prime number of rank ${rank} is ${prime}")
    }

    // if (hasUpdate) cache.saveBinary(outputCacheFileName)

  }

}
