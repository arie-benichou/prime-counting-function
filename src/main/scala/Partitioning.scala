import scala.collection.mutable
import PrimesUtils.{findPrimeBefore, findPrimeAfter, getPrimesBetween}

case object Partitioning {

  val NullRange = Range(0, 0)
  val NullSide = new Side(0, NullRange, 0)
  val NullMiddle = new Middle(0, 0)

  case class Side(
      val prime: Long,
      val range: Range,
      val polarityHint: Int = 0
  ) {
    val isNull: Boolean = this.range == NullRange
    lazy val polarity: Int =
      if (range.start > range.end) -1
      else if (range.start < range.end) 1
      else polarityHint
    lazy val isNegative = polarity < 0
    lazy val unsignedRange =
      if (this.isNegative) this.range.swap else this.range
    lazy val capacity: Long =
      if (isNegative) range.start - range.end + 1
      else if (isNull) 0
      else range.end - range.start + 1
    lazy val isSingleton = capacity == 1
    def setPolarityHint(hint: Int) = copy(polarityHint = hint)
    def swap() = copy(range = range.swap, polarityHint = -polarityHint)
    private def start(newStart: Long) = copy(range = range.start(newStart))
    private def end(newEnd: Long) = copy(range = range.end(newEnd))
    def offsetStartBy(offset: Int) =
      start(range.start + offset).setPolarityHint(polarity)
    def offsetEndBy(offset: Int) =
      end(range.end + offset).setPolarityHint(polarity)
    override def toString: String = if (isNull) "{}" else range.toString
  }

  // TODO use composition with Range, like as Side ?
  class Middle(val p1: Long, val p2: Long) extends Range((p1 * p1), (p2 * p2)) {
    override def toString: String =
      if (isSingleton) s"{${this.start}}"
      else s"[${p1} x ${p1} ; ${p2} x ${p2}]"
  }

  sealed case class PartitioningState(
      initialRange: Range,
      left: Side = NullSide,
      middle: Middle = NullMiddle,
      right: Side = NullSide
  ) {
    def setLeft(newLeft: Side) = copy(left = newLeft)
    def setMiddle(newMiddle: Middle) = copy(middle = newMiddle)
    def setRight(newRight: Side) = copy(right = newRight)
    override def toString: String = s"${left} # ${middle} # ${right}"
  }

  def splitEdge(edge: Long): (Side, Side) = {
    val squareRoot = math.sqrt(edge).toLong
    val primeBefore =
      if (squareRoot == 1) 1
      else findPrimeBefore(squareRoot + 1)
    val primeAfter =
      if (squareRoot == 2) 3
      else findPrimeAfter(squareRoot - 1)
    (
      new Side(primeAfter, Range(edge, primeAfter * primeAfter)),
      new Side(primeBefore, Range(primeBefore * primeBefore, edge))
    )
  }

  def pass1(state: PartitioningState): PartitioningState = {
    val (upperBound, lowerBound) = splitEdge(state.initialRange.start)
    if (upperBound.capacity <= lowerBound.capacity)
      state
        .setLeft(upperBound)
        .setMiddle(
          new Middle(upperBound.prime, 0)
        )
    else
      state
        .setLeft(lowerBound.swap)
        .setMiddle(
          new Middle(lowerBound.prime, 0)
        )
  }

  def pass2(state: PartitioningState) = {
    val (upperBound, lowerBound) = splitEdge(state.initialRange.end)
    if (upperBound.capacity <= lowerBound.capacity)
      state
        .setRight(upperBound.swap)
        .setMiddle(new Middle(state.middle.p1, upperBound.prime))
    else
      state
        .setRight(lowerBound)
        .setMiddle(new Middle(state.middle.p1, lowerBound.prime))
  }

  def pass3(state: PartitioningState) = {
    if (state.left.isSingleton)
      state.setLeft(NullSide)
    else state
  }

  def pass4(state: PartitioningState) = {
    if (state.right.isSingleton)
      state.setRight(NullSide)
    else state
  }

  def pass5(state: PartitioningState): PartitioningState = {
    if (state.left.isNull) state
    else {
      if (state.left.isNegative)
        state.setLeft(state.left.offsetStartBy(-1))
      else
        state.setLeft(state.left.offsetEndBy(-1))
    }
  }

  def pass6(state: PartitioningState): PartitioningState = {
    if (state.right.isNull) state
    else {
      if (state.right.isNegative)
        state.setRight(state.right.offsetEndBy(1))
      else
        state.setRight(state.right.offsetStartBy(1))
    }
  }

  def format(partitions: Partitions) = {
    var data = ""
    if (partitions.isWorthy) {
      val left = partitions.left
      if (!left.isNull) {
        data += (if (left.isNegative) "- " else "") + left
      }
      val middle = partitions.middle
      data += (if (left.isNull) "" else " + ") + middle
      val right = partitions.right
      if (!right.isNull) {
        data += (if (right.isNegative) " - " else " + ") + right
      }
    } else {
      val initialRange = partitions.initialRange
      data += initialRange.toString
    }
    data
  }

  sealed case class Partitions(initialRange: Range) {

    lazy val state =
      pass6(
        pass5(
          pass4(
            pass3(
              pass2(
                pass1(
                  new PartitioningState(this.initialRange)
                )
              )
            )
          )
        )
      )

    lazy val left = state.left
    lazy val middle = state.middle
    lazy val right = state.right

    lazy val hasMiddlePart = this.middle.capacity > 1
    lazy val isWorthy =
      initialRange.capacity - (
        left.capacity + right.capacity
      ) >= 0

    lazy val isApplicable = hasMiddlePart && isWorthy

    lazy val traces =
      s"range  : $initialRange \n" +
        s" ->    : ${this.state} \n" +
        s" ->    : ${format(this)} \n" +
        s"worthy : ${this.isWorthy} \n" +
        (if (isWorthy) "" else " -> " + initialRange.toString)

    override lazy val toString: String = format(this)
  }

  def of(range: Range) = {
    if (range.start > range.end)
      throw new Exception(
        s"$range is not a valid range : ${range.start} > ${range.end}"
      )
    if (range.start < 1)
      throw new Exception(
        s"$range is not a valid range : ${range.start} < 1"
      )
    new Partitions(range) // TODO rename into Partitioning
  }

  def apply(range: Range): Partitions = Partitioning.of(range)

  // TODO PrimeCounter concern
  val setOfRanges = mutable.Set[Range]()

  // TODO PrimeCounter concern
  for (c <- (1 to 8).combinations(2)) {
    val (start, end) = (c(0), c(1))
    val partitioning = Partitioning.of(Range(start, end))
    if (partitioning.isWorthy) {
      if (!partitioning.left.isNull && !partitioning.left.isSingleton)
        setOfRanges.addOne(partitioning.left.unsignedRange)
      if (!partitioning.right.isNull && !partitioning.right.isSingleton)
        setOfRanges.addOne(partitioning.right.unsignedRange)
    } else setOfRanges.addOne(partitioning.initialRange)
  }

  // TODO PrimeCounter concern
  val RangeSingularities = Map[Range, Int](
    (setOfRanges
      .map(x => {
        (x, getPrimesBetween(x.start, x.end).size) // TODO count
      }))
      .filter(_._2 != 0)
      .toSeq: _*
  )

  def main(args: Array[String]): Unit = {
    val (start, end) = (args(0).toLong, args(1).toLong)
    val partitioning = Partitioning.of(Range(start, end))
    println(partitioning.traces)
    println(partitioning)
  }

}
