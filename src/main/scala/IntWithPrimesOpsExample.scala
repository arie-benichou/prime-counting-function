object IntWithPrimesOpsExample {

  def main(args: Array[String]): Unit = {

    import IntWithPrimesOps._

    for (x <- 1 to Int.MaxValue by 1000000) {
      println(s"$x : ${x.phi}")
    }

    println

    val x = Int.MaxValue

    println(s"$x : ${x.phi}")
    println(s"prime from max rank ${IntWithPrimesOps.MaxRank} : ${Int.primeFromRank(x.phi)}")

  }

}
