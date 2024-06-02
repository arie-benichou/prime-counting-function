case object Range {

  def moduloForward(alignee: Long, aligner: Long): Long = {
    val remainder = alignee % aligner
    if (remainder == 0) alignee else alignee + (aligner - remainder)
  }

  def moduloBackward(alignee: Long, aligner: Long): Long = {
    val remainder = alignee % aligner
    if (remainder == 0) alignee else alignee - remainder
  }

  def apply(tuple: (Long, Long)): Range = apply(tuple._1, tuple._2)

  def apply(end: Long): Range = new Range(1, end)

  def apply(start: Long, end: Long): Range = new Range(start, end)

}

case class Range(start: Long, end: Long) {

  lazy val polarity: Int = if (start > end) -1 else 1

  lazy val capacity: Long = (end - start) + 1

  lazy val isSingleton = capacity == 1

  def swap(): Range = copy(start = this.end, end = this.start)

  def end(newEnd: Long): Range = copy(end = newEnd)

  def start(newStart: Long): Range = copy(start = newStart)

  def alignStart(n: Long): Range = start(Range.moduloForward(start, n))

  def alignEnd(n: Long): Range = end(Range.moduloBackward(end, n))

  def %(n: Long): Range = alignStart(n).alignEnd(n)

  // for faster % operation
  def alignedEdgesOn(n: Long) =
    (Range.moduloForward(this.start, n), Range.moduloBackward(this.end, n))

  override def toString: String =
    if (isSingleton) s"{${start}}" else s"[$start ; $end]"

}
