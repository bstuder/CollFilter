import scala.collection.mutable.ArrayBuffer
import scala.math._
/*
 * A Matrix representation mainly used to perform
 * matrix multiplication and LU factorisation
 */
class Matrix(els: List[List[Float]]) {
  import Matrix._
  
  val elements: List[List[Float]] = els
  def nRows: Int = elements.length
  def nCols: Int = elements match {
    case Nil => 0
    case x :: xs => x.length
  }

  //Check for well formed matrix
  require(elements.forall(_.length == nCols))

  private def addRows(a: List[Float],
    b: List[Float]): List[Float] = (a, b).zipped map (_ + _)

  private def subRows(a: List[Float],
    b: List[Float]): List[Float] = (a, b).zipped map (_ - _)

  def +(other: Matrix): Matrix = {
    require((other.nRows == nRows) && (other.nCols == nCols))
    new Matrix((elements, other.elements).zipped.map(addRows(_, _)))
  }

  def -(other: Matrix): Matrix = {
    require((other.nRows == nRows) && (other.nCols == nCols))
    new Matrix((elements, other.elements).zipped.map(subRows(_, _)))
  }

  def transpose: Matrix = new Matrix(elements.transpose)

  def *(other: Matrix): Matrix = {
    require(nCols == other.nRows)
    val t = other.transpose
    new Matrix(
      for (row <- elements) yield {
        for (otherCol <- t.elements)
          yield Matrix.dotVectors(row, otherCol)
      })
  }

  // optimisation for the case : this * this.transpose + weigthedIdentity(coef)
  def dotTranspose(coef: Float): Matrix = {
    val t1 = System.currentTimeMillis
    val indexedElem = ArrayBuffer.concat(elements)
    val t2 = System.currentTimeMillis
    def mapIdx(i: Int,j: Int): Float = {
      if(i > j) Matrix.dotVectors(indexedElem(i), indexedElem(j)) 
      else if(i == j) 0.5f * (coef + Matrix.dotVectors(indexedElem(i), indexedElem(i)))
      else 0f
    }
    //-----------------------------------------------------
    //val halfMat = new Matrix(List.tabulate(nRows, nRows)(mapIdx))
    val halfArr = ArrayBuffer.fill(nRows, nRows)(0f)
    for(i <- 0 until nRows ; j <- 0 to i) {
        halfArr(i).update(j, mapIdx(i, j))
    }
    val halfMat = new Matrix(halfArr.toList map (_.toList))
    //-----------------------------------------------------
    val t3 = System.currentTimeMillis
    val cmat = halfMat + halfMat.transpose
    val t4 = System.currentTimeMillis
    Matrix.timing = (Matrix.timing zip ((t2-t1)::(t3-t2)::(t4-t3)::Nil)).map(x => x._1 + x._2)
    cmat
  }

  // Solve the case : this * x = other with LU decompositon of this
  def LUsolve(other: Matrix): List[Float] = {
    require(other.nRows == nCols && other.nCols == 1)
    val cl = this.cholesky 
    val c0 = ArrayBuffer.concat(other.elements.flatten)
    val c1 = diagSolve(cl._1, c0, false)
    val c2 = diagSolve(cl._2, c1, true)
    c2.toList
  }

  /* 
   * A faster version of the Doolittle LU decompostion
   * May not be safe for side cases
   */
  def fastLU: (DenseMatrix, DenseMatrix) = {
    require(nCols == nRows)

    val L: DenseMatrix = ArrayBuffer.fill(nRows, nRows)(0f)
    val U: DenseMatrix = ArrayBuffer.fill(nRows, nRows)(0f)

    def parSum(i: Int, j: Int, n: Int): Float = {
      var sum = 0f
      if(n > 0) {
        for(m <- (0 until n))
          sum += L(i)(m) * U(m)(j)
      }
      sum
    }

    def slvL(i: Int, j: Int): Float =
      if(i < j) 0 
      else if(i == j) 1 
      else (elements(i)(j) - parSum(i,j,j)) / U(j)(j)

    def slvU(i: Int, j: Int): Float =
      if(i > j) 0 
      else elements(i)(j) - parSum(i,j,i)

    for(i <- (0 until nRows)) {
      for(j <- (0 until nRows)) {
        L(i).update(j, slvL(i, j))
        U(i).update(j, slvU(i, j))
      }
    }

    (L, U)
  }
  
  /*
   * Cholesky decomposition (LU equivalent for Hermitian matrix)
   */
   def cholesky: (DenseMatrix, DenseMatrix) = {
    require(nCols == nRows)
    
    val C: DenseMatrix = ArrayBuffer.fill(nRows, nRows)(0f)
    
    def parSum(i: Int, j: Int): Float = {
      var sum = 0f
      if(j > 0) {
        for(k <- (0 to j-1))
          sum += C(i)(k) * C(j)(k)
      }
      sum
    }

    def slvC(i: Int, j: Int): Float = 
      if(i > j)
        (elements(i)(j) - parSum(i, j)) / C(j)(j)
      else if(i == j)
        sqrt(elements(i)(i) - parSum(i,i)).toFloat
      else 0
    
    // should proceed row by row (implementation seemes to do that)
    for(i <- (0 until nRows)) {
      for(j <- (0 to i)) {
        C(i).update(j, slvC(i, j))
      }
    }
    
    (C, C.transpose)
   }
  
  // Solve the case mat * vect = other IF this is diagonal
  private def diagSolve(mat: DenseMatrix, other: DenseVector, diagUp: Boolean): DenseVector = {
    require(other.size == nCols)
    
    var vect : DenseVector = ArrayBuffer.fill(nRows)(0f)

    def getX(i: Int) = {
      (other(i) - Matrix.dotVectors(mat(i), vect)) / mat(i)(i)
    }
    
    if(diagUp) {
      for(i <- (0 to nRows - 1))
        vect.update(nRows - 1 - i, getX(nRows - 1 - i))
    } else {
       for(i <- (0 to nRows - 1))
        vect.update(i, getX(i))
    }
    
    vect
  }

  override def toString: String = {
    val rowStrings =
      for (row <- elements)
        yield row.mkString("[", ", ", "]")
    rowStrings.mkString("", "\n", "\n")
  }
}

object Matrix {
  type DenseMatrix = ArrayBuffer[ArrayBuffer[Float]]
  type DenseVector = ArrayBuffer[Float]
  
  var timing: List[Long] = List.fill(3)(0L)
  
  def apply(elements: List[List[Float]]): Matrix = new Matrix(elements)
  
  def apply(dmat: DenseMatrix): Matrix = {
    def mapIdx(i: Int, j: Int) = dmat(i)(j)
    if(dmat.size > 0)
      Matrix(List.tabulate(dmat.size, dmat(0).size)(mapIdx))
    else
      Matrix(Nil)
  }
  
  // the identity matrix
  def identity(size: Int): Matrix = weightIdentity(size)(1)

  // the weighted identity matrix (the diagonal contains the coef value)
  def weightIdentity(size: Int)(coef: Float): Matrix = {
    Matrix(List.tabulate(size, size)((i, j) => if(i == j) coef else 0))
  }

  def dotVectors(a: List[Float], b: List[Float]): Float = {
    (a, b).zipped map (_ * _) reduce (_ + _)
  }
  
  def dotVectors(a: DenseVector, b: DenseVector): Float = {
    (a, b).zipped map (_ * _) reduce (_ + _)
  }
  
}
