case object PrimeCouple {

  def apply(primeCouple: (Int, Int)): PrimeCouple = apply(primeCouple._1, primeCouple._2)

  def apply(p1: Int, p2: Int): PrimeCouple = new PrimeCouple(p1, p2)

}

sealed case class PrimeCouple(p1: Int, p2: Int) {

  lazy val asTuple = (p1, p2)

  lazy val spanningRange = Range(p1 * p1, p2 * p2 - 1)

  lazy val next = PrimeCouple(this.p2, PrimeUtils.findPrimeAfter(this.p2))

  override def toString: String = s"(${p1}, ${p2}) -> [${this.spanningRange.start}; ${this.spanningRange.end + 1}["
}