import scala.collection.mutable.ArrayBuffer
import scala.math._

/*
 * A Matrix representation mainly used to perform
 * matrix multiplication and LU factorisation
 */
import Matrix._
class Matrix(els: DenseMatrix) {
  val elements: DenseMatrix = els
  def nRows: Int = elements.size
  def nCols: Int = if(nRows > 0) elements(0).size else 0

  //Check for well formed matrix
  require(elements.forall(_.length == nCols))

  private def addRows(a: DenseVector, b: DenseVector): DenseVector = 
    (a zip b) map (x => x._1 + x._2)
    
  private def subRows(a: DenseVector, b: DenseVector): DenseVector = 
    (a zip b) map (x => x._1 - x._2)

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
    // the lower triangular mapping for multiplication
    def mapIdx(i: Int,j: Int): Float = {
      if(i > j) Matrix.dotVectors(elements(i), elements(j)) 
      else if(i == j) 0.5f * (coef + Matrix.dotVectors(elements(i), elements(i)))
      else 0f
    }
    
    val t1 = System.currentTimeMillis
    // construct the lower half matrix
    val halfArr = ArrayBuffer.fill(nRows, nRows)(0f)
    var i = 0
    var j = 0
    while(i < nRows) {
      while(j <= i) {
        halfArr(i) update (j, mapIdx(i, j))
        j += 1
      }
      i += 1
      j = 0
    }
    val halfMat = new Matrix(halfArr)
    val t2 = System.currentTimeMillis
    // complete the matrix ba summing the lower and upper half
    val cmat = halfMat + halfMat.transpose
    val t3 = System.currentTimeMillis
    Matrix.timing = (Matrix.timing zip ((t2-t1)::(t3-t2)::Nil)).map(x => x._1 + x._2)
    cmat
  }

  // Solve the case : this * x = other with Cholesky decompositon
  def CLsolve(other: Matrix): DenseVector = {
    require(other.nRows == nCols && other.nCols == 1)
    val cl = this.cholesky 
    val c0 = other.elements.flatten
    val c1 = diagSolve(cl._1, c0, false)
    val c2 = diagSolve(cl._2, c1, true)
    c2
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
        var k = 0
        while(k < j) {
          sum += C(i)(k) * C(j)(k)
          k += 1
        }
      }
      sum
    }

    def slvC(i: Int, j: Int): Float = 
      if(i > j)
        (elements(i)(j) - parSum(i, j)) / C(j)(j)
      else if(i == j)
        sqrt(elements(i)(i) - parSum(i,i)).toFloat
      else 0
    
    // should proceed row by row
    var i = 0
    var j = 0
    while(i < nRows) {
      while(j <= i) {
        C(i) update (j, slvC(i, j))
        j += 1
      }
      i += 1
      j = 0
    }
    
    (C, C.transpose)
   }
  
  // Solve the case mat * vect = other IF this is diagonal
  private def diagSolve(mat: DenseMatrix, other: DenseVector, diagUp: Boolean): DenseVector = {
    require(other.size == nCols)
    
    val vect : DenseVector = ArrayBuffer.fill(nRows)(0f)

    def getX(i: Int) = {
      (other(i) - Matrix.dotVectors(mat(i), vect)) / mat(i)(i)
    }
    
    if(diagUp) {
      var i = nRows - 1
      while(i >= 0){
        vect update (i, getX(i))
        i -= 1
      }
    } else {
      var i = 0
      while(i < nRows){
        vect update (i, getX(i))
        i += 1
      }
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
  
  var timing: List[Long] = List.fill(2)(0L)
  
  def apply(elements: List[List[Float]]): Matrix = 
    new Matrix(ArrayBuffer.concat(elements) map (line => ArrayBuffer.concat(line)))
  
  def apply(dmat: DenseMatrix): Matrix = new Matrix(dmat)
  
  // the identity matrix
  def identity(size: Int): Matrix = weightIdentity(size)(1)

  // the weighted identity matrix (the diagonal contains the coef value)
  def weightIdentity(size: Int)(coef: Float): Matrix = {
    Matrix(ArrayBuffer.tabulate(size, size)((i, j) => if(i == j) coef else 0))
  }

  def dotVectors(a: List[Float], b: List[Float]): Float = {
    (0f /: (a zip b))((buf, x) => buf + x._1 * x._2)
  }
  
  // most(?) optimized way to compute vectors product
  def dotVectors(a: DenseVector, b: DenseVector): Float = {
    var buf = 0f
    var i = 0
    while(i < a.size) {
      buf += a(i) * b(i)
      i += 1
    }
    buf
  }
  
}
