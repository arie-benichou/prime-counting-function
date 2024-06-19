object DistinctComposites {

  sealed case class DistinctComposites(numberOfBits: Long) {

    private val numberOfWords =
      (numberOfBits / 64 + (if (numberOfBits % 64 == 0) 0 else 1))

    assert(
      numberOfWords <= Int.MaxValue,
      "\n" + "number of bits can not be greater than 137 438 953 408"
    )

    private val bytes = new Array[Long](numberOfWords.toInt)

    lazy val capacity: Long = 64 * numberOfWords

    @inline
    def set(index: Int): Unit = {
      val bitMask = 1L << (index & 0x3f)
      bytes(index >> 6) |= bitMask
    }

    // TODO could use fold
    def cardinality(): Int = {
      var sum = 0
      var i = 0
      while (i < numberOfWords) {
        sum += java.lang.Long.bitCount(bytes(i))
        i += 1
      }
      sum
    }

  }

}
