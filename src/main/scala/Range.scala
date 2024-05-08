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

import Range._

sealed case class Range(start: Int, end: Int) {

  private def end(newEnd: Int): Range = copy(end = newEnd)

  private def start(newStart: Int): Range = copy(start = newStart)

  private def alignStart(n: Int): Range = start(moduloForward(start, n))

  private def alignEnd(n: Int): Range = end(moduloBackward(end, n))

  override def toString: String = s"[$start, $end]"

  lazy val capacity: Int = (end - start) + 1

  def %(n: Int): Range = alignStart(n).alignEnd(n)

}