import java.util.Arrays

object BitSet {

  // minimalist bitset version inspired by :
  // org/apache/spark/util/collection/BitSet.scala

  class BitSet(numberOfBits: Int) extends Serializable {

    @inline
    private def bit2bytes(numberOfBits: Int) = ((numberOfBits - 1) >> 6) + 1

    private val bytes = new Array[Long](bit2bytes(numberOfBits))
    private val numberOfWords = bytes.length

    lazy val capacity: Int = numberOfWords * 64

    @inline
    def set(index: Int): Unit = {
      val bitMask = 1L << (index & 0x3f)
      bytes(index >> 6) |= bitMask
    }

    @inline
    def unset(index: Int): Unit = {
      val bitMask = 1L << (index & 0x3f)
      bytes(index >> 6) &= ~bitMask
    }

    @inline
    def get(index: Int): Boolean = {
      val bitMask = 1L << (index & 0x3f)
      (bytes(index >> 6) & bitMask) != 0
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
