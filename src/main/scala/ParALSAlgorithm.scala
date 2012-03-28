import scala.collection.parallel.mutable.ParHashMap
import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeSet
import scala.collection.Set
import scala.math._
import Constants._

/*
 * The parallel implementation of ALS algorithm using parallel Hash map
 */
class ParALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float) {
  def this(dataSetInit: DataSetInitializer) = this(dataSetInit,NF,LAMBDA)
  def this(dataSetInit: DataSetInitializer, Nf: Int) = this(dataSetInit,Nf,LAMBDA)
  def this(dataSetInit: DataSetInitializer, lambda: Float) = this(dataSetInit,NF,lambda)

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
  var m: ParHashMap[Int, List[Float]] = new ParHashMap()
  var u: ParHashMap[Int, List[Float]] = new ParHashMap()

  def init = {
    dsi.init
    m ++= dsi.setUpM(Nf)
    for(i <- 1 to dsi.usrToMov.size) {
      u += ((i, Nil))
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
    if(m.size == 0) {
      init
    }
    solveU
    solveM
    // Timers
    val timerH = timer map hRead
    val sum = timer reduce (_ + _)
    println("\n(¯`·._.·CUMMULATIVE STEP TIME·._.·´¯)" +
    "\n\t[mu]Ratings : " + timerH(0) +
    "\n\tmat[MU]i : " + timerH(1) +
    "\n\tA[ij] : " + timerH(2) +
    "\n\tRij : " + timerH(3) +
    "\n\tV[ij] : " + timerH(4) +
    "\n\tLU : " + timerH(5) +
    "\n\n\tTotal : " + hRead(sum) + "\n")
  }

  def solveU = {
    def solveUsr(i: Int): List[Float] = {
      if(!dsi.usrToMov.contains(i)) {
        return List.fill(Nf)(0)
      }
      val ratedSet: Set[Int] = dsi.usrToMov(i).keySet
      val t1 = tick
      val mRatings: List[List[Float]] = ratedSet.toList.sortWith(_ < _) map (m(_))
      val t2 = tick
      val matMi = new Matrix(mRatings.transpose)
      val t3 = tick
      val Ai = matMi.dotTranspose(mRatings.size * lambda)
      val t4 = tick
      val Rij = new Matrix(dsi.usrToMov(i).toList.sortWith(_._1 < _._1).map(_._2 :: Nil))
      val t5 = tick
      val Vi = matMi * Rij
      val t6 = tick
      val ui = Ai.LUsolve(Vi)
      val t7 = tick
      val loctimes = (t2-t1) :: (t3-t2) :: (t4-t3) :: (t5-t4) :: (t6-t5) :: (t7-t6) :: Nil
      timer = (timer zip loctimes) map (x => x._1 + x._2)
      ui.elements.flatten
    }
    println("[ALS] Solving U")
    u foreach (tup => u update (tup._1, solveUsr(tup._1)))
  }

  def solveM = {
    def solveMov(j: Int): List[Float] = {
      if(!dsi.movToUsr.contains(j)) {
        return List.fill(Nf)(0)
      }
      val ratedSet: Set[Int] = dsi.movToUsr(j).keySet
      val t1 = tick
      val uRatings: List[List[Float]] = ratedSet.toList.sortWith(_ < _) map (u(_))
      val t2 = tick
      val matUi = new Matrix(uRatings.transpose)
      val t3 = tick
      val Aj = matUi.dotTranspose(uRatings.size * lambda)
      val t4 = tick
      val Rij = new Matrix(dsi.movToUsr(j).toList.sortWith(_._1 < _._1).map(_._2 :: Nil))
      val t5 = tick
      val Vj = matUi * Rij
      val t6 = tick
      val mj = Aj.LUsolve(Vj)
      val t7 = tick
      val loctimes = (t2-t1) :: (t3-t2) :: (t4-t3) :: (t5-t4) :: (t6-t5) :: (t7-t6) :: Nil
      timer = (timer zip loctimes) map (x => x._1 + x._2)
      mj.elements.flatten
    }
    println("[ALS] Solving M")
    m foreach (tup => m update (tup._1, solveMov(tup._1)))
  }

  def checkNorm: Float = {
    def delta(user: List[Float], movie: List[Float], expectedR: Float): Float = {
      val delt = Matrix.dotVectors(user,movie) - expectedR
      delt * delt
    }
     print("[ALS] Frobenius norm : ")
     var norm = 0f
     for(i <- (1 to dsi.usrToMov.size)) {
       dsi.usrToMov(i) foreach (tup => norm += delta(u(i),m(tup._1), tup._2))
     }
     val rmse = sqrt(norm/dsi.nbrRatings).toFloat
     norm = sqrt(norm).toFloat
     println(norm + " (RMSE : " + rmse + ")" + "\n")
     norm
  }

}
