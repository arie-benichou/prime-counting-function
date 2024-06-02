import scala.collection.mutable

case class CacheManager(
    filename: String,
    notYetMemoizedValue: Long,
    initialData: ((Long, Long), Long)*
) {

  var isLoaded = false

  private val map: mutable.Map[(Long, Long), Long] =
    mutable
      .Map[(Long, Long), Long](initialData: _*)
      .withDefaultValue(notYetMemoizedValue)

  def load(): Long = {
    if (!this.isLoaded && os.isFile(os.pwd / filename)) {
      val json = ujson.read(os.read(os.pwd / filename))
      val cachedResults =
        upickle.default.read[mutable.Map[(Long, Long), Long]](json)
      val size = cachedResults.size
      var i = 0
      for (cachedResult <- cachedResults) {
        i += 1
        map.addOne(cachedResult)
      }
      this.isLoaded = true
      i
    } else 0
  }

  def save() = {
    os.write.over(
      os.pwd / filename,
      upickle.default.write(mutable.SortedMap[(Long, Long), Long]().addAll(map))
    )
  }

  def update(primesCouple: (Long, Long), numberOfPrimes: Long): Long = {
    map.addOne(primesCouple, numberOfPrimes)
    numberOfPrimes
  }

  def apply(tupleOfPrimes: (Long, Long)): Long = map(tupleOfPrimes)

  def clear() = map.clear()

}
