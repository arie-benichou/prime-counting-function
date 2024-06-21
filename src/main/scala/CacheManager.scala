import scala.collection.mutable

import upickle.default._

case class CacheManager(
    notYetMemoizedValue: Long,
    initialData: (Long, Long)*
) extends Iterable[Long] {

  // private 
  val map: mutable.SortedMap[Long, Long] =
    mutable
      .SortedMap[Long, Long](initialData: _*)
      .withDefaultValue(notYetMemoizedValue)

  def clear() = {
    map.clear()
    map.addAll(initialData)
    this
  }

  /*
   * TODO refactoring
   * TODO : pagination
   * when n > Int.MaxValue
   */
  def loadBinary(filename: String): Long = {
    if (os.isFile(os.pwd / filename)) {
      println(
        s"${Console.GREEN}Loading cache from ${filename}${Console.RESET}"
      )
      val binary = os.read.bytes(os.pwd / filename)
      val cachedResults = readBinary[mutable.SortedMap[Long, Long]](binary)
      val size = cachedResults.size
      var i = 0
      for (cachedResult <- cachedResults) {
        i += 1
        map.addOne(cachedResult)
      }
      println(
        s"${Console.GREEN}$i primes loaded from cache${Console.RESET}"
      )
      i
    } else 0
  }

  /*
   * TODO refactoring
   * TODO : pagination
   * when n > Int.MaxValue
   */
  def loadJson(filename: String): Long = {
    if (os.isFile(os.pwd / filename)) {
      println(
        s"${Console.GREEN}Loading cache from ${filename}${Console.RESET}"
      )
      val json = ujson.read(os.read(os.pwd / filename))
      val cachedResults = read[mutable.SortedMap[Long, Long]](json)
      val size = cachedResults.size
      var i = 0
      for (cachedResult <- cachedResults) {
        i += 1
        map.addOne(cachedResult)
      }
      println(
        s"${Console.GREEN}$i primes loaded from cache${Console.RESET}"
      )
      i
    } else 0
  }

  // TODO refactoring
  def saveBinary(filename: String) = {
    os.write.over(
      os.pwd / filename,
      upickle.default.writeBinary(map)
    )
    println(
      s"${Console.GREEN}cache is being saved in ${filename}${Console.RESET}"
    )
  }

  // TODO refactoring
  def saveJson(filename: String) = {
    os.write.over(
      os.pwd / filename,
      upickle.default.write(map)
    )
    println(
      s"${Console.GREEN}cache is being saved in ${filename}${Console.RESET}"
    )
  }

  @inline
  def update(prime: Long, numberOfPrimes: Long): Long = {
    map.addOne(prime, numberOfPrimes)
    numberOfPrimes
  }

  @inline
  def apply(prime: Long): Long = map(prime)

  def iterator = map.keySet.iterator

}
