import scala.collection.mutable

object Primes2357 {

  val Primes = List(2, 3, 5, 7)

  val MapForFastCounting = {
    val odd = mutable.ListBuffer[Int]()
    val even = mutable.ListBuffer[Int]()
    for (i <- 1 to 4) {
      val combinations = Primes.combinations(i)
      val products = combinations.map(_.product)
      if (i % 2 == 1) odd.addAll(products)
      else even.addAll(products)
    }
    Map[String, List[Int]](
      "+" -> odd.toList,
      "-" -> even.toList
    )
  }

  val MapForMultiplesOf2357: Map[Long, Boolean] =
    (0 until List(2, 3, 5, 7).product)
      .map(i => (i.toLong, !PrimesUtils.isMultipleOf2357(i)))
      .toMap[Long, Boolean]

  @inline
  def doesNotdivide(n: Long): Boolean = MapForMultiplesOf2357(n % 210)

  @inline
  def divides(n: Long): Boolean = !MapForMultiplesOf2357(n % 210)

  @inline
  def numberOfMultiplesIn(range: Range): Long = { // TODO rename multiplesIn()
    var positives = 1L
    for (n <- MapForFastCounting("+")) {
      val (start, end) = range.alignedEdgesOn(n)
      positives += (end - start) / n
    }
    var negatives = 0L
    for (n <- MapForFastCounting("-")) {
      val (start, end) = range.alignedEdgesOn(n)
      negatives += (end - start) / n
    }
    positives - negatives
  }

  def main(args: Array[String]): Unit = {
    println(Primes2357.MapForFastCounting)
    println(Primes2357.numberOfMultiplesIn(Range(1, 35)))
    println(Primes2357.MapForMultiplesOf2357)
  }

}
