import scala.collection.mutable

object PrimesCouple {

  val NotMemoizedYet: Int = -1

  val CacheFileName = "data.json"

  type CachedResultType = mutable.SortedMap[(Int, Int), Int]

  private val map: mutable.SortedMap[(Int, Int), Int] = mutable.SortedMap[(Int, Int), Int](
    (1, 2) -> 2,
    (2, 3) -> 2
  )

  def getFromCache(primesCouple: PrimesCouple): Int = getFromCache(primesCouple.asTuple)

  def getFromCache(tupleOfPrimes: (Int, Int)): Int = map.getOrElse(tupleOfPrimes, NotMemoizedYet)

  def updateCache(primesCouple: PrimesCouple, numberOfPrimes: Int): Int = {
    updateCache(primesCouple.asTuple, numberOfPrimes)
  }

  def updateCache(primesCouple: (Int, Int), numberOfPrimes: Int): Int = {
    map.addOne(primesCouple, numberOfPrimes)
    numberOfPrimes
  }

  def updateCache(cachedResult: ((Int, Int), Int)): Int = {
    map.addOne(cachedResult)
    cachedResult._2
  }

  def saveCache(filename: String = CacheFileName): Unit = {
    val jsonString = upickle.default.write(map)
    os.write.over(os.pwd / filename, jsonString)
  }

  def loadCache(filename: String = CacheFileName): Unit = {
    if (os.isFile(os.pwd / filename)) {
      println(s"${Console.GREEN}Loading cache : ")
      val json = ujson.read(os.read(os.pwd / "data.json"))
      val cachedResults = upickle.default.read[CachedResultType](json)
      print("\u001b[%dA\u001b[2K".format(1)) // Move up 2 times and erase line content
      val size = cachedResults.size
      var i = 0
      for (cachedResult <- cachedResults) {
        i += 1
        print(s"\rLoading cache : ${math.round(i.toDouble / size * 100)} %   ")
        if (i % 120 == 0) Thread.sleep(0)
        updateCache(cachedResult)
      }
      print("\n\u001b[%dA\u001b[2K".format(1)) // Move up 2 times and erase line content
      println(s"Loading cache : done !")
      println(s"($size ranges loaded from cache)${Console.RESET}")
    }

  }

}

import PrimesCouple._

sealed case class PrimesCouple(primesCouple: (Int, Int)) {

  val p1: Int = primesCouple._1
  val p2: Int = primesCouple._2
  val asTuple: (Int, Int) = (p1, p2)

  var range: PrimeRange = PrimeRange(p1 * p1, p2 * p2)
  var primaryDivisorsFrom11: Iterable[Int] = Nil
  var useCache = true

  def setPrimaryDivisorsFrom11(primaryDivisorsFrom11: Iterable[Int]): PrimesCouple = {
    this.primaryDivisorsFrom11 = primaryDivisorsFrom11.takeWhile(_ <= p2)
    this
  }

  def useCache(useCache: Boolean): PrimesCouple = {
    this.useCache = useCache
    this
  }

  def countPrimes(end: Int): Int = {
    if (this.useCache) {
      val memoizedValue = getFromCache(this)
      if (memoizedValue != NotMemoizedYet) memoizedValue
      else updateCache(this, range.countPrimes(primaryDivisorsFrom11))
    }
    else range.countPrimes(primaryDivisorsFrom11)
  }

  override def toString: String = this.asTuple.toString()

}