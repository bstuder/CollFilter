import scala.collection.mutable.{ArrayBuffer, Ctrie}
import scala.math.sqrt
import Constants._
import scalala.tensor.dense._

/*
 * The serial version of ALS algorithm
 * using scalala library
 */
class LaALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Double) {
  val dsi: DataSetInitializer = dataSetInit
  var m: Ctrie[Int, List[Double]] = new Ctrie()
  var u: Ctrie[Int, List[Double]] = new Ctrie()

  def init = {
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
    solveU
    solveM
  }

  def solveU = {
    def solveUsr(i: Int): List[Double] = {
      require(dsi.usrToMov.contains(i))
      val mSorted = ArrayBuffer.concat(dsi.usrToMov(i)) sortWith (_._1 < _._1)
      val mRatings = new DenseMatrix[Double](mSorted.size, Nf, (mSorted flatMap (x => m(x._1))).toArray)
      val Ai = (mRatings.t * mRatings) + (DenseMatrix.eye[Double](Nf) *= (mSorted.size * lambda))
      val Rij = DenseVector(mSorted map(_._2) toArray)
      val Vi = mRatings.t * Rij
      val ui = DenseMatrix.DenseMatrixCanSolveDenseVector(Ai.toDense, Vi)
      ui.toList
    }
    println("[ALS] Solving U")
    u foreach (tup => u update (tup._1, solveUsr(tup._1)))
  }

  def solveM = {
    def solveMov(j: Int): List[Double] = {
      require(dsi.movToUsr.contains(j))
      val uSorted = ArrayBuffer.concat(dsi.movToUsr(j)) sortWith (_._1 < _._1)
      val uRatings = new DenseMatrix[Double](uSorted.size, Nf, (uSorted flatMap (x => u(x._1))).toArray)
      val Aj = (uRatings.t * uRatings) + (DenseMatrix.eye[Double](Nf) *= (uSorted.size * lambda))
      val Rij = DenseVector(uSorted map (_._2) toArray)
      val Vj = uRatings.t * Rij
      val mj = DenseMatrix.DenseMatrixCanSolveDenseVector(Aj.toDense, Vj)
      mj.toList
    }
    println("[ALS] Solving M")
    m foreach (tup => m update (tup._1, solveMov(tup._1)))
  }

  def checkNorm: Double = {
    def delta(user: List[Double], movie: List[Double], expectedR: Double): Double = {
      val delt = (user, movie).zipped.map(_ * _).reduce(_ + _) - expectedR
      delt * delt
    }
     var norm = 0d
     for((ukey, movieMap) <- dsi.usrToMov ; (mkey, rating) <- movieMap) {
       norm += delta(u(ukey), m(mkey), rating)
     }
     val rmse = sqrt(norm / dsi.nbrRatings)
     println("[ALS] RMSE : " + rmse + "\n")
     rmse
  }
}

object LaALSAlgorithm {
  def apply(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float): LaALSAlgorithm = {
    val alg = new LaALSAlgorithm(dataSetInit, Nf, lambda)
    alg.init
    alg
  }
  
  def apply(dataSetInit: DataSetInitializer, Nf: Int): LaALSAlgorithm = {
    LaALSAlgorithm(dataSetInit, Nf, LAMBDA)
  }
  
  def apply(dataSetInit: DataSetInitializer, lambda: Float): LaALSAlgorithm = {
    LaALSAlgorithm(dataSetInit, NF ,lambda)
  }
  
  def apply(dataSetInit: DataSetInitializer): LaALSAlgorithm = {
    LaALSAlgorithm(dataSetInit, NF, LAMBDA)
  }
}
