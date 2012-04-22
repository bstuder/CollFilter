import scala.collection.parallel.mutable.ParCtrie
import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt
import Constants._
import Matrix._

/*
 * The parallel implementation of ALS algorithm using parallel Hash map
 */
class ParALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float) {
  //------------------------------------------------------
  var timer: List[Long] = List.fill(6)(0L)
  def tick = System.currentTimeMillis

  def hRead(x: Long): String = {
    if(x > 60 * 60 * 1000)
      x/(60 * 60 * 1000) + " h " + hRead(x % (60 * 60 * 1000))
    else if(x > 60 * 1000)
      x/(60 * 1000) + " min " + hRead(x % (60 * 1000))
    else if (x > 1000)
      x / 1000 + " sec"
    else
      x + " ms"
  }
  //------------------------------------------------------

  val dsi: DataSetInitializer = dataSetInit
  val m: ParCtrie[Int, DenseVector] = new ParCtrie()
  val u: ParCtrie[Int, DenseVector] = new ParCtrie()

  def init = {
    m ++= dsi.setUpM(Nf)
    for(i <- dsi.usrToMov.keySet) {
      u += ((i, ArrayBuffer.empty))
    }
  }

  def stepN(n: Int) = {
    for(i <- (1 to n)) {
      println("[ALS] Step : " + i)
      step
      checkNorm
    }
  }

  def step = {
    solveU
    solveM
    // Timers
    val timerH = timer map hRead
    val sum = timer reduce (_ + _)
    println("\n(¯`·._.·CUMMULATIVE STEP TIME·._.·´¯)" +
    "\n\t[mu]Ratings : " + timerH(0) +
    "\n\tmat[MU]i : " + timerH(1) +
    "\n\tA[ij] : " + timerH(2) +
    "\n\t tabulate : " + hRead(Matrix.timing(0)) +
    "\n\t halfmat : " + hRead(Matrix.timing(1)) +
    "\n\tRij : " + timerH(3) +
    "\n\tV[ij] : " + timerH(4) +
    "\n\tCholesky : " + timerH(5) +
    "\n\n\tTotal : " + hRead(sum) + "\n")
  }

  def solveU = {
    def solveUsr(i: Int): DenseVector = {
      if(!dsi.usrToMov.contains(i)) {
        return ArrayBuffer.fill(Nf)(0)
      }
      val mSorted = ArrayBuffer.concat(dsi.usrToMov(i)) sortWith (_._1 < _._1)
      val t1 = tick
      val mRatings = mSorted map (x => m(x._1))
      val t2 = tick
      val matMi = Matrix(mRatings.transpose)
      val t3 = tick
      val Ai = matMi.dotTranspose(mRatings.size * lambda)
      val t4 = tick
      val Rij = mSorted map (_._2)
      val t5 = tick
      val Vi = matMi * Rij
      val t6 = tick
      val ui = Ai.CLsolve(Vi)
      val t7 = tick
      val loctimes = (t2-t1) :: (t3-t2) :: (t4-t3) :: (t5-t4) :: (t6-t5) :: (t7-t6) :: Nil
      timer = (timer zip loctimes) map (x => x._1 + x._2)
      ui
    }
    println("[ALS] Solving U")
    u foreach (tup => u update (tup._1, solveUsr(tup._1)))
  }

  def solveM = {
    def solveMov(j: Int): DenseVector = {
      if(!dsi.movToUsr.contains(j)) {
        return ArrayBuffer.fill(Nf)(0)
      }
      val uSorted = ArrayBuffer.concat(dsi.movToUsr(j)) sortWith (_._1 < _._1)
      val t1 = tick
      val uRatings = uSorted map (x => u(x._1))
      val t2 = tick
      val matUi = Matrix(uRatings.transpose)
      val t3 = tick
      val Aj = matUi.dotTranspose(uRatings.size * lambda)
      val t4 = tick
      val Rij = uSorted map (_._2)
      val t5 = tick
      val Vj = matUi * Rij
      val t6 = tick
      val mj = Aj.CLsolve(Vj)
      val t7 = tick
      val loctimes = (t2-t1) :: (t3-t2) :: (t4-t3) :: (t5-t4) :: (t6-t5) :: (t7-t6) :: Nil
      timer = (timer zip loctimes) map (x => x._1 + x._2)
      mj
    }
    println("[ALS] Solving M")
    m foreach (tup => m update (tup._1, solveMov(tup._1)))
  }

  def checkNorm: Float = {
    def delta(user: DenseVector, movie: DenseVector, expectedR: Float): Float = {
      val delt = Matrix.dotVectors(user, movie) - expectedR
      delt * delt
    }
     print("[ALS] Frobenius norm : ")
     var norm = 0f
     for((ukey, movieMap) <- dsi.usrToMov ; (mkey, rating) <- movieMap) {
       norm += delta(u(ukey), m(mkey), rating)
     }
     val rmse = sqrt(norm / dsi.nbrRatings).toFloat
     norm = sqrt(norm).toFloat
     println(norm + " (RMSE : " + rmse + ")" + "\n")
     norm
  }
}

object ParALSAlgorithm {
  def apply(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float): ParALSAlgorithm = {
    val alg = new ParALSAlgorithm(dataSetInit, Nf, lambda)
    alg.init
    alg
  }
  
  def apply(dataSetInit: DataSetInitializer, Nf: Int): ParALSAlgorithm = {
    ParALSAlgorithm(dataSetInit, Nf, LAMBDA)
  }
  
  def apply(dataSetInit: DataSetInitializer, lambda: Float): ParALSAlgorithm = {
    ParALSAlgorithm(dataSetInit, NF ,lambda)
  }
  
  def apply(dataSetInit: DataSetInitializer): ParALSAlgorithm = {
    ParALSAlgorithm(dataSetInit, NF, LAMBDA)
  }
}
