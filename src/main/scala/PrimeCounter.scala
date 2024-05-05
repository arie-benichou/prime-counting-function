import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._

object PrimeCounter {

  var isCacheLoaded = false

  def apply(
      end: Int,
      useCache: Boolean = true,
      useParallelism: Boolean = true,
      saveCache: Boolean = false
  ): Int = {
    if (end < 0) throw new Exception(s"$end is not a valid integer !")
    apply((1, end), useCache, useParallelism, saveCache)
  }

  private def apply(
      tuple: (Int, Int),
      useMemoization: Boolean,
      useParallelism: Boolean,
      saveCache: Boolean
  ): Int = {
    apply(PrimeRange(tuple), useMemoization, useParallelism, saveCache)
  }

  // TODO configuration object or builder
  private def apply(
      initialRange: PrimeRange,
      useCache: Boolean,
      useParallelism: Boolean,
      saveCache: Boolean
  ): Int = {

    val squareRoot = math.floor(math.sqrt(initialRange.end)).toInt

    if (useCache && !isCacheLoaded) {
      PrimesCouple.loadCache()
      isCacheLoaded = true
    }

    if (squareRoot < 5)
      return PrimeUtils
        .getPrimesBetween(initialRange.start, initialRange.end)
        .length

    val involvedPrimes = PrimeUtils.getPrimesBetween(3, squareRoot)
    val primesAfter2357 = involvedPrimes.dropWhile(_ < 11)
    val twoByTwo = involvedPrimes.zip(involvedPrimes.drop(1))

    val buffer = new ListBuffer[PrimesCouple]

    for (tupleOfPrimes <- twoByTwo) {
      val primesCouple = PrimesCouple(tupleOfPrimes)
      primesCouple.setPrimaryDivisorsFrom11(primesAfter2357)
      primesCouple.useCache(useCache)
      buffer.addOne(primesCouple)
    }

    val result0 =
      PrimesCouple.getFromCache((1, 2)) + PrimesCouple.getFromCache((2, 3))

    val result1 =
      if (useParallelism) buffer.par.map(_.countPrimes).sum
      else buffer.map(_.countPrimes).sum

    val result2 =
      if (buffer.nonEmpty) {
        val lastPrimeCouple = buffer.last
        if (initialRange.end > lastPrimeCouple.range.end) {
          val endingRange =
            PrimeRange(lastPrimeCouple.range.end, initialRange.end)
          endingRange.countPrimes(lastPrimeCouple.primaryDivisorsFrom11)
        } else 0
      } else 0

    // TODO inject cache filename from configuration object
    // TODO check if cache is "dirty"
    if (useCache && saveCache) PrimesCouple.saveCache()

    result0 + result1 + result2

  }

  def main(args: Array[String]): Unit = {
    println(apply(100))
    println(apply(1000))
    println(apply(10000))
    println(apply(100000))
    println(apply(1000000))
    println(apply(10000000))
    println(apply(100000000))
    println(apply(1000000000))
    println(apply(2000000000))
    println(apply(46340 * 46340))
  }

}
