import scala.collection.mutable.{ArrayBuffer, Ctrie}
import scala.collection.Map
import scala.util.Random

/*
 * Read the stream of information (userId, movieId, rating)
 * and populate 2 sparse Matrix (as Map of Map)
 */
class DataSetInitializer(src: Stream[(Int, Int, Float)]) {
  import Constants._

  val usrToMov = new Ctrie[Int, Ctrie[Int, Float]]
  val movToUsr = new Ctrie[Int, Ctrie[Int, Float]]

  val nbrRatings: Int = src.length

  //format is : (UserID, MovieID, Rating)
  def init {
    def addInnerKey(map: Ctrie[Int, Ctrie[Int, Float]],
      oKey: Int, iKey: Int, value: Float) = {
      map.get(oKey) match {
        case Some(imap) => imap += ((iKey, value))
        case None => {
          //adding a new map
          val tmp = new Ctrie[Int, Float]
          tmp += ((iKey, value))
          map += ((oKey, tmp))
        }
      }
    }

    println("[Dsi/init] Building Users/Movies Map")
    var cnt = 0
    for ((uid, mid, rat) <- src) {
      cnt += 1
      if(cnt % incr == 0) {
        print("\r" + (cnt * 100 / nbrRatings) +"%")
      }
      addInnerKey(usrToMov, uid, mid, rat)
      addInnerKey(movToUsr, mid, uid, rat)
    }
    println("\r100%")

    println("[Dsi/init] Building complete (" +
      usrToMov.size + " users, " +
      movToUsr.size + " movies, " +
      nbrRatings + " ratings)")
  }

  def setUpM(Nf: Int): Map[Int, ArrayBuffer[Float]] = {
    val r = new Random()
    def rand: Float = r.nextFloat() * MAXRAND
    def randList = ArrayBuffer.fill(Nf - 1)(rand)
    // Average on the first row, small random value on the other
    movToUsr mapValues (x => randList.+:((0f /: x)(_ + _._2) / x.size))
  }
  
  def setUpMD(Nf: Int): Map[Int, List[Double]] = {
    val r = new Random()
    def rand: Double = r.nextDouble() * MAXRAND
    def randList: List[Double] = List.fill(Nf - 1)(rand)
    movToUsr mapValues (x => ((0d /: x)(_ + _._2) / x.size) :: randList)
  }
}

object DataSetInitializer {
  def apply(src: Stream[(Int, Int, Float)]): DataSetInitializer = {
    val dsi = new DataSetInitializer(src)
    dsi.init
    dsi
  }
}
