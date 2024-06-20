object DistinctComposites {

  sealed case class DistinctComposites(numberOfBits: Long) {

    assert(
      numberOfBits <= Int.MaxValue * 64L,
      "\n" +
        "number of bits greater than 137 438 953 408 " +
        "is currently not supported"
    )

    private val numberOfWords =
      (numberOfBits / 64 + (if (numberOfBits % 64 == 0) 0 else 1)).toInt

    private val bytes = new Array[Long](numberOfWords)

    lazy val capacity: Long = 64 * numberOfWords

    @inline
    def set(index: Long): Unit = bytes((index >> 6).toInt) |= (1L << (index & 0x3f))
    
    def cardinality(): Int = {
      (0 until numberOfWords).foldLeft(0) { (sum, i) =>
        sum + java.lang.Long.bitCount(bytes(i))
      }
    }

  }

}
