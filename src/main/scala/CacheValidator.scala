import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

import PrimesUtils._
import PrimesCounter._

object CacheValidator {

  def apply(start: Long, end: Long) = {

    val errors = ListBuffer[PrimesCouple]()
    val missings = ListBuffer[PrimesCouple]()

    println
    println("validating cache...")
    println

    @tailrec
    def loop(p1: Long): Unit = {
      if (p1 <= end) {
        val pc = PrimesCouple(p1, findPrimeAfter(p1))
        print(pc + " -> ")
        val nonp = countOtherNonPrimes(pc.spanningRange, pc.p1)
        val np = calculateNumberOfPrimes(pc.spanningRange, nonp)
        val storedValue = cache(pc.p1)
        if (storedValue == np) {
          print(Console.GREEN)
          print("ok")
          print(Console.RESET)
        } else if (cache(pc.p1) == cache.notYetMemoizedValue) {
          missings += pc
          print(Console.YELLOW)
          print("missing")
          print(Console.RESET)
        } else if (pc.p1 < 3 && storedValue == 2) {
          print(Console.GREEN)
          print("ok")
          print(Console.RESET)
        } else {
          errors += pc
          print(Console.RED)
          print("failed")
          print(Console.RESET)
        }
        println
        loop(pc.p2)
      }
    }
    loop(start)

    if (missings.nonEmpty) {
      println
      println("List of missing entries : ")
      println
      print(Console.YELLOW)
      missings.foreach(println)
      print(Console.RESET)
    }

    if (errors.nonEmpty) {
      println
      println("List of erroneous entries : ")
      println
      print(Console.RED)
      errors.foreach(println)
      print(Console.RESET)
    }

    println
    println("number of missing entries : " + missings.size)
    println
    println("number of erroneous entries : " + errors.size)
    println

  }
 
  def main(args: Array[String]): Unit = {
    cache.loadBinary("data-to-validate.bin")
    this(args(0).toLong, args(1).toLong)
  }

}