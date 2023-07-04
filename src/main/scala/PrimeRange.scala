case object PrimeRange {

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

  private def isMultipleOf2357(n: Int): Boolean = {
    for (prime <- List(2, 3, 5, 7)) if (n % prime == 0) return true
    false
  }

  def apply(tuple: (Int, Int)): PrimeRange = new PrimeRange(tuple._1, tuple._2)

  private def apply(start: Int, end: Int): PrimeRange = new PrimeRange(start, end)

}

import PrimeRange._

sealed case class PrimeRange(start: Int, end: Int) {

  lazy val capacity: Int = (end - start) + 1

  def %(n: Int): PrimeRange = alignStart(n).alignEnd(n)

  def end(newEnd: Int): PrimeRange = copy(end = newEnd)

  def start(newStart: Int): PrimeRange = copy(start = newStart)

  private def alignStart(n: Int): PrimeRange = start(moduloForward(start, n))

  private def alignEnd(n: Int): PrimeRange = end(moduloBackward(end, n))

  lazy val multiplesOf2357: Int = {
    var counter = 0
    for (n <- this.start to this.end if isMultipleOf2357(n)) counter += 1
    counter
  }

  def NonPrimesAndNotMultiplesOf2357(prime: Int): Seq[Int] = {
    val alignedRange = this % prime
    for (n <- alignedRange.start to alignedRange.end by prime if !isMultipleOf2357(n)) yield n
  }

  def countPrimes(primes: Iterable[Int]): Int = (
    this.capacity
      - this.multiplesOf2357
      - primes.flatMap(NonPrimesAndNotMultiplesOf2357).toSet.size
    )

  override def toString: String = s"[$start, $end]"
}

