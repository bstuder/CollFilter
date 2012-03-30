import scala.collection.mutable.ArrayBuffer
/*
 * A Matrix representation mainly used to perform
 * matrix multiplication and LU factorisation
 */
class Matrix(els: List[List[Float]]) {
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
    val indexedElem = ArrayBuffer.concat(elements)
    
    def mapIdx(i: Int,j: Int): Float = {
      if(i > j) Matrix.dotVectors(indexedElem(i), indexedElem(j)) 
      else if(i == j) 0.5f * (coef + Matrix.dotVectors(indexedElem(i), indexedElem(j)))
      else 0f
    }
    val halfMat = new Matrix(List.tabulate(nRows, nRows)(mapIdx))
    halfMat + halfMat.transpose
  }

  // Solve the case : this * x = other with LU decompositon of this
  def LUsolve(other: Matrix): Matrix = {
    require(other.nRows == nCols && other.nCols == 1)
    val lu = this.fastLU // lu._1 * lu._2 = this
    val s1 = diagSolve(lu._1, other, false) //lu._1 * s1 = other
    val s2 = diagSolve(lu._2, s1, true) // lu._2 * s2 = s1
    s2
  }

  /* 
   * A faster version of the Doolittle LU decompostion
   * May not be safe for side cases
   */
  def fastLU: (ArrayBuffer[ArrayBuffer[Float]], ArrayBuffer[ArrayBuffer[Float]]) = {
    require(nCols == nRows)

    var L: ArrayBuffer[ArrayBuffer[Float]] = ArrayBuffer.fill(nRows)(ArrayBuffer.fill(nRows)(0f))
    var U: ArrayBuffer[ArrayBuffer[Float]] = ArrayBuffer.fill(nRows)(ArrayBuffer.fill(nRows)(0f))

    def parSum(i: Int, j: Int, n: Int): Float = {
      var sum = 0f
      
      if(n > 0) {
        for(m <- (0 to n - 1))
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

    for(i <- (0 to nRows - 1)) {
      for(j <- (0 to nRows - 1)) {
        L(i).update(j, slvL(i, j))
        U(i).update(j, slvU(i, j))
      }
    }

    (L, U)
  }
  
  // Solve the case this * x = other IF this is diagonal
  private def diagSolve(vect: ArrayBuffer[ArrayBuffer[Float]], other: Matrix, diagUp: Boolean): Matrix = {
    require(other.nRows == nCols && other.nCols == 1)
    
    var x : ArrayBuffer[Float] = ArrayBuffer.fill(nRows)(0f)

    def getX(i: Int) = {
      (other.elements(i)(0) - Matrix.dotVectors(vect(i), x)) / vect(i)(i)
    }
    
    if(diagUp) {
      for(i <- (0 to nRows - 1))
        x.update(nRows - 1 - i, getX(nRows - 1 - i))
        
    } else {
       for(i <- (0 to nRows - 1))
        x.update(i, getX(i))
    }
    
    new Matrix(x.toList map (_ :: Nil))
  }

  override def toString: String = {
    val rowStrings =
      for (row <- elements)
        yield row.mkString("[", ", ", "]")
    rowStrings.mkString("", "\n", "\n")
  }
}

object Matrix {

  // the identity matrix
  def identity(size: Int): Matrix = weightIdentity(size)(1)

  // the weighted identity matrix (the diagonal contains the coef value)
  def weightIdentity(size: Int)(coef: Float): Matrix = {
    new Matrix(List.range(0,size) map (x => List.fill(size)(0f) updated(x,coef)))
  }

  def dotVectors(a: List[Float], b: List[Float]): Float = {
    (a, b).zipped.map(_ * _) reduce (_ + _)
  }
  
  def dotVectors(a: ArrayBuffer[Float], b: ArrayBuffer[Float]): Float = {
    (a, b).zipped.map(_ * _) reduce (_ + _)
  }
  
}
