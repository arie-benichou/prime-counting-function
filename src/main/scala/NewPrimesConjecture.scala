/* Conjecture 1.a
 *
 * Soit p1, p2 et p3, trois nombres premiers consecutifs
 *  Exemple : si p1 = 3, alors p2 = 5, et p3 = 7
 *
 * Quels sont les triplets (p1, p2, p3) tels que :
 *  p1 x p2 - 1 = k x p3, ou k est un entiel naturel > 0
 *
 * Il semblerait que si p1 > 17, alors il n'existerait pas de solution,
 * 17 etant le 7 eme nombre premier.
 * Et l'ensemble des triplets satisfaisant cette egalite jusque la serait :
 * { (2,3,5), (3,5,7), (17,19,23) }
 */

/* Conjecture 1.b
 *
 * Soit p1, p2 et p3, trois nombres premiers consecutifs
 *  Exemple : si p1 = 7, alors p2 = 11, et p3 = 13
 *
 * Quels sont les triplets (p1, p2, p3) tels que :
 *  p1 x p2 + 1 = k x p3, ou k est un entiel naturel > 0
 *
 * Il semblerait que si p1 > 1321, alors il n'existerait pas de solution,
 * 1321 etant le 216 eme nombre premier.
 * Et l'ensemble des triplets satisfaisant cette egalite jusque la serait :
 * { (1,2,3), (7,11,13), (31,37,41), (1321,1327,1361) }
 */

object NewPrimesConjecture {

  import scala.collection.mutable

  val ExpectedSet1a = mutable.SortedSet[(Long, Long, Long)](
    (2, 3, 5),
    (3, 5, 7),
    (17, 19, 23)
  )

  val ExpectedSet1b = mutable.SortedSet[(Long, Long, Long)](
    (7, 11, 13),
    (31, 37, 41),
    (1321, 1327, 1361)
  )

  def checkConjecture1a(triplet: (Long, Long, Long)): Boolean =
    (triplet._1 * triplet._2 - 1) % triplet._3 == 0

  def checkConjecture1b(triplet: (Long, Long, Long)): Boolean =
    (triplet._1 * triplet._2 + 1) % triplet._3 == 0

  // Aucun autre triplet de trouve en cherchant jusqu'a 100 000 000
  def main(args: Array[String]): Unit = {

    PrimesCounter.cache.loadBinary("data.bin", verbose = false)

    val set1a = mutable.SortedSet[(Long, Long, Long)]()
    val set1b = mutable.SortedSet[(Long, Long, Long)]()

    import java.util.concurrent.{Executors, TimeUnit}
    val scheduler = Executors.newScheduledThreadPool(1)
    var rank = 0L
    val task = new Runnable {
      def run = print("\r" + s"${rank} ranks checked so far...")
    }
    scheduler.scheduleAtFixedRate(task, 1, 1, TimeUnit.SECONDS)

    println

    for (i <- 1 to 10000) {
      rank = i
      val triplet = (
        PrimesCounter(i + 0),
        PrimesCounter(i + 1),
        PrimesCounter(i + 2)
      )
      if (checkConjecture1a(triplet)) {
        set1a += triplet
        println(s"rank = $i $triplet")
      }
      if (checkConjecture1b(triplet)) {
        set1b += triplet
        println(s"rank = $i $triplet")
      }

    }

    task.run
    scheduler.shutdown()
    scheduler.awaitTermination(1, TimeUnit.MINUTES)
    println

    println
    println("Conjecture 1.a : " + set1a.mkString("{", ",", "}"))
    println((ExpectedSet1a -- set1a).isEmpty)

    println
    println("Conjecture 1.b : " + set1b.mkString("{", ",", "}"))
    println((ExpectedSet1b -- set1b).isEmpty)

    println
    assert((ExpectedSet1a -- set1a).isEmpty)
    assert((ExpectedSet1b -- set1b).isEmpty)

  }

}
