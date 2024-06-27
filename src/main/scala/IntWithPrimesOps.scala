object IntWithPrimesOps {

  PrimesCounter.cache.loadBinary("data-int.bin", verbose = true)

  implicit class IntInstances(val i: Int) extends AnyVal {
    def phi(): Int = {
      require(i >= 1, s"\nargument should be >= 1")
      PrimesCounter(1, i).toInt
    }
  }

  val MaxRank = Int.MaxValue.phi

  implicit class IntObject(t: Int.type) {
    def primeFromRank(i: Int): Int = {
      require(i > 0, s"\nargument should be > 0")
      require(i <= MaxRank, s"\nargument should be <= ${MaxRank}")
      PrimesCounter(i).toInt
    }
  }

  def main(args: Array[String]): Unit = {
    val x = Int.MaxValue
    println(x)
    println(x.phi())
    println(Int.primeFromRank(x.phi()))
  }

}
