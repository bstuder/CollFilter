import scala.collection.mutable.{ArrayBuffer, Ctrie}
import scala.math.sqrt
import Constants._
import Matrix._

/*
 * The original serial ALS algorithm
 */
class ALSAlgorithm(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float) {
  val dsi: DataSetInitializer = dataSetInit
  val m: Ctrie[Int, DenseVector] = new Ctrie()
  val u: Ctrie[Int, DenseVector] = new Ctrie()

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
  }

  def solveU = {
    def solveUsr(i: Int): DenseVector = {
      require(dsi.usrToMov.contains(i))
      val mSorted = ArrayBuffer.concat(dsi.usrToMov(i)) sortWith (_._1 < _._1)
      val mRatings = mSorted map (x => m(x._1))
      val matMi = Matrix(mRatings.transpose)
      val Ai = matMi.dotTranspose(mRatings.size * lambda)
      val Rij = mSorted map (_._2)
      val Vi = matMi * Rij
      val ui = Ai.CLsolve(Vi)
      ui
    }
    println("[ALS] Solving U")
    u transform ((key, value) => solveUsr(key))
  }

  def solveM = {
    def solveMov(j: Int): DenseVector = {
      require(dsi.movToUsr.contains(j))
      val uSorted = ArrayBuffer.concat(dsi.movToUsr(j)) sortWith (_._1 < _._1)
      val uRatings = uSorted map (x => u(x._1))
      val matUi = Matrix(uRatings.transpose)
      val Aj = matUi.dotTranspose(uRatings.size * lambda)
      val Rij = uSorted map (_._2)
      val Vj = matUi * Rij
      val mj = Aj.CLsolve(Vj)
      mj
    }
    println("[ALS] Solving M")
    m transform ((key, value) => solveMov(key))
  }

  def checkNorm: Float = {
    def delta(user: DenseVector, movie: DenseVector, expectedR: Float): Float = {
      val delt = Matrix.dotVectors(user, movie) - expectedR
      delt * delt
    }
     var norm = 0f
     for((ukey, movieMap) <- dsi.usrToMov ; (mkey, rating) <- movieMap) {
       norm += delta(u(ukey), m(mkey), rating)
     }
     val rmse = sqrt(norm / dsi.nbrRatings).toFloat
     println("[ALS] RMSE : " + rmse + "\n")
     rmse
  }
}

object ALSAlgorithm {
  def apply(dataSetInit: DataSetInitializer, Nf: Int, lambda: Float): ALSAlgorithm = {
    val alg = new ALSAlgorithm(dataSetInit, Nf, lambda)
    alg.init
    alg
  }
  
  def apply(dataSetInit: DataSetInitializer, Nf: Int): ALSAlgorithm = {
    ALSAlgorithm(dataSetInit, Nf, LAMBDA)
  }
  
  def apply(dataSetInit: DataSetInitializer, lambda: Float): ALSAlgorithm = {
    ALSAlgorithm(dataSetInit, NF ,lambda)
  }
  
  def apply(dataSetInit: DataSetInitializer): ALSAlgorithm = {
    ALSAlgorithm(dataSetInit, NF, LAMBDA)
  }
}
