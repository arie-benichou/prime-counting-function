object DistinctComposites {

  sealed case class DistinctComposites(numberOfBits: Long) {

    assert(
      numberOfBits <= Int.MaxValue * 32L,
      "\n" +
        s"number of bits greater than ${(Int.MaxValue.toLong * 32)} " +
        "is currently not supported"
    )

    private val numberOfInts =
      (numberOfBits / 32 + (if (numberOfBits % 32 == 0) 0 else 1)).toInt

    private val ints = new Array[Int](numberOfInts)

    lazy val capacity: Long = 32 * numberOfInts

    @inline
    def set(index: Long): Unit = {
      val intIndex = (index >> 5).toInt // index / 32
      val bitIndex = (index & 0x1f).toInt // index % 32
      ints(intIndex) |= (1 << bitIndex)
    }

    def cardinality(): Int = {
      (0 until numberOfInts).foldLeft(0) { (sum, i) =>
        sum + java.lang.Integer.bitCount(ints(i))
      }
    }
  }

}
