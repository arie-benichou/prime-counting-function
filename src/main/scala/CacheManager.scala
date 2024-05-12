import scala.collection.mutable.SortedMap

case class CacheManager(
    filename: String,
    notYetMemoizedValue: Int,
    initialData: ((Int, Int), Int)*
) {

  var isLoaded = false

  private val map: SortedMap[(Int, Int), Int] =
    SortedMap[(Int, Int), Int](initialData: _*)
      .withDefaultValue(notYetMemoizedValue)

  def load(): Int = {
    if (!this.isLoaded && os.isFile(os.pwd / filename)) {
      val json = ujson.read(os.read(os.pwd / filename))
      val cachedResults = upickle.default.read[SortedMap[(Int, Int), Int]](json)
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
    os.write.over(os.pwd / filename, upickle.default.write(map))
  }

  def update(primesCouple: (Int, Int), numberOfPrimes: Int): Int = {
    map.addOne(primesCouple, numberOfPrimes)
    numberOfPrimes
  }

  def apply(tupleOfPrimes: (Int, Int)): Int = map(tupleOfPrimes)

  def clear() = map.clear()

}
