import scala.collection.mutable.HashMap
import scala.collection.Set
import scala.math._
import Constants._

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
	    // liste of movies rated by user i
	    val ratedSet: Set[Int] = dsi.usrToMov(i).keySet 
	    val mRatings: List[List[Float]] = m.filter(p => ratedSet.contains(p._1)).toList.sortWith(_._1 < _._1).map(_._2)
	    // matMi : matrix where only the movies rated by i are picked
	    val matMi = new Matrix(mRatings.transpose)
      val Ai = matMi * matMi.transpose + Matrix.weightIdentity(Nf)(mRatings.size * lambda)
	    // Rij : raw line of ratings form user i
	    val Rij = new Matrix(dsi.usrToMov(i).toList.sortWith(_._1 < _._1).map(tup => tup._2) :: Nil)
	    val Vi = matMi * Rij.transpose
	    val ui = Ai.LUsolve(Vi) // column vector
      ui.transpose.elements(0) // getting the first row
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
      val uRatings: List[List[Float]] = u.filter(p => ratedSet.contains(p._1)).toList.sortWith(_._1 < _._1).map(_._2)
      // matUi : matrix where only the user who rated j are picked
      val matUi = new Matrix(uRatings.transpose)
      val Aj = matUi * matUi.transpose + Matrix.weightIdentity(Nf)(uRatings.size * lambda)
      // Rij : raw column of ratings for movie j
      val Rij = new Matrix(dsi.movToUsr(j).toList.sortWith(_._1 < _._1).map(tup => tup._2) :: Nil)
      val Vj = matUi * Rij.transpose
      val mj = Aj.LUsolve(Vj)
      mj.transpose.elements(0)
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
	   // square root of the sum of all the element square
     print("[ALS] Frobenius norm : ")
     var norm = 0f
     for(i <- (1 to dsi.usrToMov.size)) {
       dsi.usrToMov(i) foreach (tup => norm += delta(u(i),m(tup._1), tup._2))
     }
     norm = sqrt(norm).toFloat
     println(norm + "\n")
     norm
	}
}
