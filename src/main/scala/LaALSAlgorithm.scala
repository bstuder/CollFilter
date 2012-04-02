import scala.collection.mutable.HashMap
import scala.collection.Set
import scala.math._
import Constants._
import scalala.tensor.dense._

/*
 * The serial version of ALS algorithm
 * using scalala library
 */
class LaALSAlgorithm(dsi: DataSetInitializer, Nf: Int, lambda: Double) {

  def this(dsi: DataSetInitializer) = this(dsi,NF,LAMBDA)
  def this(dsi: DataSetInitializer, Nf: Int) = this(dsi,Nf,LAMBDA)
  def this(dsi: DataSetInitializer, lambda: Double) = this(dsi,NF,lambda)

  var m: HashMap[Int, List[Double]] = new HashMap()
  var u: HashMap[Int, List[Double]] = new HashMap()

  def init = {
    dsi.init
    m ++= dsi.setUpMD(Nf)
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
  def stepUntil(threshold: Double) = {
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
    def solveUsr(i: Int): List[Double] = {
      if(!dsi.usrToMov.contains(i)) {
        return List.fill(Nf)(0)
      }
      val ratedSet: Set[Int] = dsi.usrToMov(i).keySet
      val mRatings = new DenseMatrix[Double](ratedSet.size, Nf, (ratedSet.toList.sortWith(_ < _) flatMap (m(_))).toArray)
      val Ai = (mRatings.t * mRatings) + (DenseMatrix.eye[Double](Nf) *= (ratedSet.size * lambda))
      val Rij = DenseVector(dsi.usrToMov(i).toList.sortWith(_._1 < _._1).map(_._2).toArray)
      val Vi = mRatings.t * Rij
      val ui = DenseMatrix.DenseMatrixCanSolveDenseVector(Ai.toDense, Vi)
      ui.toList
    }
    println("[ALS] Solving U")
    u foreach (tup => u update (tup._1, solveUsr(tup._1)))
  }

  def solveM = {
    def solveMov(j: Int): List[Double] = {
      if(!dsi.movToUsr.contains(j)) {
        return List.fill(Nf)(0)
      }
      val ratedSet: Set[Int] = dsi.movToUsr(j).keySet
      val uRatings = new DenseMatrix[Double](ratedSet.size, Nf, (ratedSet.toList.sortWith(_ < _) flatMap (u(_))).toArray)
      val Aj = (uRatings.t * uRatings) + (DenseMatrix.eye[Double](Nf) *= (ratedSet.size * lambda))
      val Rij = DenseVector(dsi.movToUsr(j).toList.sortWith(_._1 < _._1).map(_._2).toArray)
      val Vj = uRatings.t * Rij
      val mj = DenseMatrix.DenseMatrixCanSolveDenseVector(Aj.toDense, Vj)
      mj.toList
    }
    println("[ALS] Solving M")
    m foreach (tup => m update (tup._1, solveMov(tup._1)))
  }

  /* We calculate the Frobenius norm between our guessed rating
   * and the provided ratings : u[i] * m[j] =~ r[i][j] */
  def checkNorm: Double = {
    def delta(user: List[Double], movie: List[Double], expectedR: Double): Double = {
      val delt = (user, movie).zipped.map(_ * _).reduce(_ + _) - expectedR
      delt * delt
    }
     print("[ALS] Frobenius norm : ")
     var norm = 0d
     for(i <- (1 to dsi.usrToMov.size)) {
       dsi.usrToMov(i) foreach (tup => norm += delta(u(i),m(tup._1), tup._2))
     }
     val rmse = sqrt(norm/dsi.nbrRatings)
     norm = sqrt(norm)
     println(norm + " (RMSE : " + rmse + ")" + "\n")
     norm
  }
}
