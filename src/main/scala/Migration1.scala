import scala.collection.mutable

import PrimesUtils._

object Migration1 {

  def migrateCache(): Unit = {
    val cache = new CacheManager(
      Long.MinValue,
      PrimesCounter.Singularities.map(s => (s._1.p1, s._2)): _*
    )
    val newMap: mutable.SortedMap[Long, Long] = mutable.SortedMap[Long, Long]()
    val n = cache.loadJson("data-big.json")
    println(n)
    for (key <- cache) {
      val (p1, nonp) = (key, cache(key))
      val p2 = findPrimeAfter(p1)
      val pc = PrimesCouple(p1, p2)
      val n = PrimesCounter.calculateNumberOfPrimes(pc.spanningRange, nonp)
      println(pc + " -> " + nonp + " -> " + n)
      newMap.addOne((key, n))
    }
    os.write.over(os.pwd / "data-big.json", upickle.default.write(newMap))
  }

  def main(args: Array[String]): Unit = {
    return migrateCache()
    // println(cache.loadBinary("data.bin"))
  }

}
