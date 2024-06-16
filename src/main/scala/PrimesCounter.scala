import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import scala.annotation.tailrec

import PrimesUtils.{isPrime, findPrimeBefore, findPrimeAfter}
import Partitioning.{Partitions, Side, Middle, RangeSingularities}

object PrimesCounter {

  private val FifthPrime = 11L

  private val AvailableProcessors = Runtime.getRuntime.availableProcessors

  val Singularities = Seq(
    (PrimesCouple(1, 2), 2L),
    (PrimesCouple(2, 3), 2L)
  )

  private var hasUpdate = false

  val cache = new CacheManager(
    Long.MinValue,
    Singularities.map(s => (s._1.p1, s._2)): _*
  )

  /*
   * TODO : pagination
   * when n > Int.MaxValue
   */
  private def countOtherNonPrimes(range: Range, maxPrimeDivisor: Long): Long = {

    if (range.capacity >= Int.MaxValue)
      throw new Exception("Array indexing limit reached")

    val data = new Array[Boolean](range.capacity.toInt)

    @inline
    def mark(onp: Long) = data((onp % range.start).toInt) = true

    case class Task(range: Range, prime: Long) extends Runnable {
      lazy val (start, end) = range.alignedEdgesOn(prime)
      def run =
        for (x <- start to end by prime if Primes2357.doesNotdivide(x)) mark(x)
    }

    val executorService: ExecutorService =
      Executors.newFixedThreadPool(AvailableProcessors)

    var primeDivisor = maxPrimeDivisor
    while (primeDivisor >= FifthPrime) {
      executorService.submit(new Task(range, primeDivisor))
      primeDivisor = findPrimeBefore(primeDivisor)
    }

    executorService.shutdown()
    executorService.awaitTermination(1, TimeUnit.DAYS)

    data.count(_ == true)
  }

  @inline
  private def countOtherNonPrimes(primesCouple: PrimesCouple): Long =
    countOtherNonPrimes(primesCouple.spanningRange, primesCouple.p1)

  @inline
  def calculateNumberOfPrimes(
      range: Range,
      numberOfOtherNonPrimes: Long
  ): Long = range.capacity -
    (Primes2357.numberOfMultiplesIn(range) + numberOfOtherNonPrimes)

  @inline
  def updateCache(primesCouple: PrimesCouple, n: Long): Long =
    cache.update(primesCouple.p1, n)

  private def memoizedNumberOfPrimes(primesCouple: PrimesCouple): Long = {
    val memoizedValue = cache(primesCouple.p1)
    if (memoizedValue != cache.notYetMemoizedValue) memoizedValue
    else {
      hasUpdate = true
      val numberOfOtherNonPrimes = countOtherNonPrimes(primesCouple)
      val numberOfPrimes = calculateNumberOfPrimes(
        primesCouple.spanningRange,
        numberOfOtherNonPrimes
      )
      println(s"$primesCouple -> $numberOfOtherNonPrimes -> $numberOfPrimes")
      updateCache(primesCouple, numberOfPrimes)
    }
  }

  private def evaluate(middle: Middle): Long = {
    if (middle.isSingleton) 0
    else {
      var primesCouple = PrimesCouple(middle.p1, findPrimeAfter(middle.p1))
      var numberOfPrimes = memoizedNumberOfPrimes(primesCouple)
      while (primesCouple.p2 < middle.p2) {
        primesCouple =
          PrimesCouple(primesCouple.p2, findPrimeAfter(primesCouple.p2))
        numberOfPrimes += memoizedNumberOfPrimes(primesCouple)
      }
      numberOfPrimes
    }
  }

  private def evaluate(range: Range, lastPrimeInvolved: Long): Long = {
    if (RangeSingularities.isDefinedAt(range)) RangeSingularities(range)
    else if (range.isSingleton)
      if (isPrime(range.start)) 1 else 0
    else
      calculateNumberOfPrimes(
        range,
        countOtherNonPrimes(range, lastPrimeInvolved)
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
    if (start < 0)
      throw new Exception(s"$start is not a valid integer !")
    if (end < 0)
      throw new Exception(s"$end is not a valid integer !")
    if (start > end)
      throw new Exception(s"[$start ; $end] is not a valid range !")
    evaluate(Partitioning.of(Range(start, end)))
  }

  // TODO ? prime from rank with apply(x)
  def apply(end: Long): Long = apply(1, end)

  def primeFromRank(rank: Long): Long = {

    if (rank < 0) throw new Exception(s"$rank is not a valid integer !")
    if (rank == 0) throw new Exception("rank 0 is not defined.")

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
    val average = math.log(range.start + range.capacity / 2)
    val diff = numberOfPrimesInthisRange - (numberOfPrimes - rank)

    val tmp = (range.start + average * diff).toLong
    val estimate = if (tmp % 2 == 0) tmp - 1 else tmp

    val prime = if (isPrime(estimate)) estimate else findPrimeAfter(estimate)
    val partitioning = Partitioning.of(Range(range.start, prime))
    val delta = diff - evaluate(partitioning)

    // println(s"estimate : $tmp")
    // println(s"delta with rank : $delta")

    val primeFromRank =
      if (delta < 0) before(prime, delta)
      else if (delta > 0) after(prime, delta)
      else prime

    // println(s"delta with original estimate : ${primeFromRank - estimate}")

    primeFromRank
  }

  // TODO R&D with this alternative
  def isPrimeAlternative1(n: Long) = {
    PrimesCounter(n) - PrimesCounter(n - 1) == 1
  }

  // TODO R&D with this alternative
  def isPrimeAlternative2(n: Long) = {
    primeFromRank(PrimesCounter(n)) == n
  }

  def main(args: Array[String]): Unit = {

    cache.loadBinary("data.bin")
    // cache.clear()

    val arg0 = if (args.isDefinedAt(0)) args(0).toLong else 350
    val arg1 = if (args.isDefinedAt(1)) args(1).toLong else -1

    if (arg1 == -1) {
      val prime = primeFromRank(arg0)
      println(s"The prime number of rank ${arg0} is ${prime}")
      val partitioning = Partitioning.of(Range(1, prime))
      println(
        s"Between ${1} and ${prime} : ${evaluate(partitioning)} primes"
      )
    } else {
      val (start, end) = (arg0, arg1)
      val partitioning = Partitioning.of(Range(start, end))
      val rank = evaluate(partitioning)
      println(
        s"Between ${start} and ${end} : ${rank} primes"
      )
      val prime = primeFromRank(rank)
      println(s"The prime number of rank ${rank} is ${prime}")
    }

    if (hasUpdate) cache.saveBinary("data-new.bin")

  }

}
