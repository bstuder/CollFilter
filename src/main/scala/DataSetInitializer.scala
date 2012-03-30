import scala.collection.mutable.HashMap
import scala.collection.Map
import scala.util.Random

/*
 * Read the stream of information (userId,movieId,rating)
 * and popuilate 2 sparse Matrix (as Map of Map)
 */
class DataSetInitializer(src: Stream[(Int, Int, Float)]) {
  import Constants._

  val usrToMov = new HashMap[Int, HashMap[Int, Float]]
  val movToUsr = new HashMap[Int, HashMap[Int, Float]]

  val nbrRatings: Int = src.length

  //format is : (UserID, MovieID, Rating)
  def init {
    def addInnerKey(map: HashMap[Int, HashMap[Int, Float]],
      oKey: Int, iKey: Int, value: Float) = {
      map.get(oKey) match {
        case Some(imap) => imap += ((iKey, value))
        case None => {
          //adding a new map
          val tmp = new HashMap[Int, Float]
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
        print("\r" + (cnt*100/nbrRatings) +"%")
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

  def setUpM(Nf: Int): Map[Int, List[Float]] = {
    val r = new Random()
    def rand: Float = r.nextFloat() * MAXRAND // random between 0 -> MAXRAND
    def randList: List[Float] = List.range(0, Nf - 1, 1) map (x => rand)
    val fRow = movToUsr mapValues (x => x.foldLeft(0f)((sum, a) => sum + a._2) / x.size)
    // Average on the first row, small random value on the other
    val M = fRow mapValues(x => x :: randList)
    M
  }
}
