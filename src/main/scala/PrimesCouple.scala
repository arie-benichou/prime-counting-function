case object PrimesCouple {

  def apply(primeCouple: (Long, Long)): PrimesCouple =
    apply(primeCouple._1, primeCouple._2)

  def apply(p1: Long, p2: Long): PrimesCouple =
    new PrimesCouple(p1, p2)

}

sealed case class PrimesCouple(p1: Long, p2: Long) {

  lazy val asTuple = (p1, p2)

  lazy val spanningRange = Range(p1 * p1, p2 * p2 - 1)

  override def toString: String =
    s"(${p1}, ${p2}) -> [${this.spanningRange.start}; ${this.spanningRange.end + 1}["
}
