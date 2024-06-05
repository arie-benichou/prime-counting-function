import scala.collection.mutable

case class CacheManager(
    filename: String,
    notYetMemoizedValue: Long,
    initialData: (Long, Long)*
) {

  var isLoaded = false

  private val map: mutable.Map[Long, Long] =
    mutable
      .Map[Long, Long](initialData: _*)
      .withDefaultValue(notYetMemoizedValue)

  def load(): Long = {
    if (!this.isLoaded && os.isFile(os.pwd / filename)) {
      val json = ujson.read(os.read(os.pwd / filename))
      val cachedResults =
        upickle.default.read[mutable.Map[Long, Long]](json)
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
      upickle.default.write(mutable.SortedMap[Long, Long]().addAll(map))
    )
  }

  @inline
  def update(prime: Long, numberOfPrimes: Long): Long = {
    map.addOne(prime, numberOfPrimes)
    numberOfPrimes
  }

  @inline
  def apply(prime: Long): Long = map(prime)

  def clear() = map.clear()

}
