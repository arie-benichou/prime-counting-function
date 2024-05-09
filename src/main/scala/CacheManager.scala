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
      println(s"${Console.GREEN}Loading cache from ${filename}")
      val json = ujson.read(os.read(os.pwd / filename))
      val cachedResults = upickle.default.read[SortedMap[(Int, Int), Int]](json)
      val size = cachedResults.size
      var i = 0
      for (cachedResult <- cachedResults) {
        i += 1
        print(s"\r${math.round(i.toDouble / size * 100)} %   ")
        map.addOne(cachedResult)
      }
      print("\n\u001b[%dA\u001b[2K".format(1)) // Move up and erase line
      println(s"$size prime couples loaded from cache ! ${Console.RESET}")
      this.isLoaded = true
      i
    } else 0
  }

  def save() = {
    os.write.over(os.pwd / filename, upickle.default.write(map))
    println(s"${Console.GREEN}Cache saved in ${filename}")
  }

  def update(primesCouple: (Int, Int), numberOfPrimes: Int): Int = {
    map.addOne(primesCouple, numberOfPrimes)
    numberOfPrimes
  }

  def apply(tupleOfPrimes: (Int, Int)): Int = map(tupleOfPrimes)

  def clear() = map.clear()

}
