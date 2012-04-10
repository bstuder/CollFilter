import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.Set
import scala.math._
import Constants._
import Matrix._

/*
 * The original serial ALS algorithm
 */
class ALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float) {
  def this(dataSetInit: DataSetInitializer) = this(dataSetInit,NF,LAMBDA)
  def this(dataSetInit: DataSetInitializer, Nf: Int) = this(dataSetInit,Nf,LAMBDA)
  def this(dataSetInit: DataSetInitializer, lambda: Float) = this(dataSetInit,NF,lambda)

  val dsi: DataSetInitializer = dataSetInit
  var m: HashMap[Int, DenseVector] = new HashMap()
  var u: HashMap[Int, DenseVector] = new HashMap()

  def init = {
    dsi.init
    m ++= dsi.setUpM(Nf)
    for(i <- 1 to dsi.usrToMov.size) {
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
    if(m.size == 0) {
      init
    }
    solveU
    solveM
  }

  def solveU = {
    def solveUsr(i: Int): DenseVector = {
      if(!dsi.usrToMov.contains(i)) {
        return ArrayBuffer.fill(Nf)(0)
      }
      val ratedSet: Set[Int] = dsi.usrToMov(i).keySet
      val mRatings: DenseMatrix = ArrayBuffer.concat(ratedSet).sortWith(_ < _) map (m(_))
      val matMi = Matrix(mRatings.transpose)
      val Ai = matMi.dotTranspose(mRatings.size * lambda)
      val Rij = Matrix(ArrayBuffer.concat(dsi.usrToMov(i)).sortWith(_._1 < _._1).map(x => ArrayBuffer(x._2)))
      val Vi = matMi * Rij
      val ui = Ai.CLsolve(Vi)
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
      val ratedSet: Set[Int] = dsi.movToUsr(j).keySet
      val uRatings: DenseMatrix = ArrayBuffer.concat(ratedSet).sortWith(_ < _) map (u(_))
      val matUi = Matrix(uRatings.transpose)
      val Aj = matUi.dotTranspose(uRatings.size * lambda)
      val Rij = Matrix(ArrayBuffer.concat(dsi.movToUsr(j)).sortWith(_._1 < _._1).map(x => ArrayBuffer(x._2)))
      val Vj = matUi * Rij
      val mj = Aj.CLsolve(Vj)
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
     for(i <- (1 to dsi.usrToMov.size)) {
       dsi.usrToMov(i) foreach (tup => norm += delta(u(i),m(tup._1), tup._2))
     }
     val rmse = sqrt(norm/dsi.nbrRatings).toFloat
     norm = sqrt(norm).toFloat
     println(norm + " (RMSE : " + rmse + ")" + "\n")
     norm
  }
}
