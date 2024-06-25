object DistinctComposites {

  sealed case class DistinctComposites(numberOfBits: Int) {

    private val array =
      new Array[Int](numberOfBits / 32 + (if (numberOfBits % 32 == 0) 0 else 1))

    @inline
    def set(n: Int): Unit = array(n >> 5) |= (1 << (n & 0x1f))

    @inline
    def count() =
      array.foldLeft(0) { (sum, int) =>
        sum + Integer.bitCount(int)
      }

  }

  // TODO unit tests
  def main(args: Array[String]): Unit = {
    println()
    println("initialization...")
    val bigSet = new DistinctComposites(Int.MaxValue)
    println("initialized !")
    println()
    val range = Range(1L, Int.MaxValue)
    println(range + " -> " + range.capacity)
    println("setting all bits...")
    for (i <- range.start to range.end) bigSet.set((i - range.start).toInt)
    println("all bits have been set !")
    println()
    println("counting...")
    val count = bigSet.count
    println(count)
    assert(count == Int.MaxValue)
    println("done !")
    println()
  }

}
