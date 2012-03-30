import scala.collection.mutable.HashMap
import scala.collection.Set
import scala.math._
import Constants._

/*
 * The original serial ALS algorithm
 */
class ALSAlgorithm(dsi: DataSetInitializer, Nf: Int, lambda: Float) {

  def this(dsi: DataSetInitializer) = this(dsi,NF,LAMBDA)
  def this(dsi: DataSetInitializer, Nf: Int) = this(dsi,Nf,LAMBDA)
  def this(dsi: DataSetInitializer, lambda: Float) = this(dsi,NF,lambda)

  var m: HashMap[Int, List[Float]] = new HashMap()
  var u: HashMap[Int, List[Float]] = new HashMap()

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

  //step until the stopping criterion is satisfied
  def stepUntil(threshold: Float) = {
    do{
      step
    } while(threshold < checkNorm)
  }

  def step = {
    if(m.size == 0) {
      init
    }
    //fix M solve U
    solveU
    //fix U solve M
    solveM
  }

  def solveU = {
    def solveUsr(i: Int): List[Float] = {
      if(!dsi.usrToMov.contains(i)) {
        return List.fill(Nf)(0)
      }
      val ratedSet: Set[Int] = dsi.usrToMov(i).keySet
      val mRatings: List[List[Float]] = ratedSet.toList.sortWith(_ < _) map (m(_))
      val matMi = new Matrix(mRatings.transpose)
      val Ai = matMi.dotTranspose(mRatings.size * lambda)
      val Rij = new Matrix(dsi.usrToMov(i).toList.sortWith(_._1 < _._1).map(_._2 :: Nil))
      val Vi = matMi * Rij
      val ui = Ai.LUsolve(Vi)
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
      val uRatings: List[List[Float]] = ratedSet.toList.sortWith(_ < _) map (u(_))
      val matUi = new Matrix(uRatings.transpose)
      val Aj = matUi.dotTranspose(uRatings.size * lambda)
      val Rij = new Matrix(dsi.movToUsr(j).toList.sortWith(_._1 < _._1).map(_._2 :: Nil))
      val Vj = matUi * Rij
      val mj = Aj.LUsolve(Vj)
      mj.elements.flatten    
    }
    println("[ALS] Solving M")
    m foreach (tup => m update (tup._1, solveMov(tup._1)))
  }

  /* We calculate the Frobenius norm between our guessed rating
   * and the provided ratings : u[i] * m[j] =~ r[i][j] */
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
