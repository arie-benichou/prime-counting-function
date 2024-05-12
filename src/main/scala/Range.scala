case object Range {

  private def moduloForward(alignee: Int, aligner: Int): Int = {
    val remainder = alignee % aligner
    if (remainder == 0) return alignee
    alignee + (aligner - remainder)
  }

  private def moduloBackward(alignee: Int, aligner: Int): Int = {
    val remainder = alignee % aligner
    if (remainder == 0) return alignee
    alignee - remainder
  }

  def apply(tuple: (Int, Int)): Range = apply(tuple._1, tuple._2)

  def apply(start: Int, end: Int): Range = new Range(start, end)

}

sealed case class Range(start: Int, end: Int) {

  lazy val capacity: Int = (end - start) + 1

  private def end(newEnd: Int): Range = copy(end = newEnd)

  private def start(newStart: Int): Range = copy(start = newStart)

  def alignStart(n: Int): Range = start(Range.moduloForward(start, n))

  def alignEnd(n: Int): Range = end(Range.moduloBackward(end, n))

  def %(n: Int): Range = alignStart(n).alignEnd(n)

  override def toString: String = s"[$start; $end]"

}
